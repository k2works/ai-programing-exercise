// src/api/__tests__/financial-statement-api.test.ts
import { describe, it, expect, beforeAll, afterAll, beforeEach } from 'vitest'
import { FastifyInstance } from 'fastify'
import { buildApp } from '../application'
import { TestDatabase } from '../../test-setup/database'
import { IncomeStatementItemDto } from '../../infrastructure/web/dto/FinancialStatementResponseDto'

describe('財務諸表 API 統合テスト', () => {
  let app: FastifyInstance
  let testDb: TestDatabase

  beforeAll(async () => {
    testDb = new TestDatabase()
    await testDb.start()
    app = await buildApp({ prisma: testDb.prisma })
  }, 60000)

  afterAll(async () => {
    await app.close()
    await testDb.stop()
  })

  beforeEach(async () => {
    await testDb.cleanup()
  })

  describe('GET /statements/balance-sheet', () => {
    it('正常系: 貸借対照表を取得できる', async () => {
      if (!testDb.prisma) throw new Error('Prisma not initialized')

      // テストデータの準備
      await testDb.prisma.account.createMany({
        data: [
          {
            accountCode: '1100',
            accountName: '現金',
            accountType: '資産',
            sumAccount: false,
            aggregationTarget: true,
            bsplDistinction: 'B'
          },
          {
            accountCode: '2100',
            accountName: '買掛金',
            accountType: '負債',
            sumAccount: false,
            aggregationTarget: true,
            bsplDistinction: 'B'
          },
          {
            accountCode: '3100',
            accountName: '資本金',
            accountType: '純資産',
            sumAccount: false,
            aggregationTarget: true,
            bsplDistinction: 'B'
          }
        ]
      })

      await testDb.prisma.monthlyAccountBalance.createMany({
        data: [
          {
            fiscalYearMonth: '202401',
            accountCode: '1100',
            openingBalance: 0,
            debitAmount: 500000,
            creditAmount: 0,
            closingBalance: 500000
          },
          {
            fiscalYearMonth: '202401',
            accountCode: '2100',
            openingBalance: 0,
            debitAmount: 0,
            creditAmount: 200000,
            closingBalance: 200000
          },
          {
            fiscalYearMonth: '202401',
            accountCode: '3100',
            openingBalance: 0,
            debitAmount: 0,
            creditAmount: 300000,
            closingBalance: 300000
          }
        ]
      })

      // API リクエスト
      const response = await app.inject({
        method: 'GET',
        url: '/statements/balance-sheet?asOfDate=2024-01-31'
      })

      // アサーション
      expect(response.statusCode).toBe(200)
      const body = JSON.parse(response.body)
      expect(body.success).toBe(true)
      expect(body.data.balanceSheet).toBeDefined()

      const balanceSheet = body.data.balanceSheet
      expect(balanceSheet.asOfDate).toBe('2024-01-31')
      expect(balanceSheet.assets).toHaveLength(1)
      expect(balanceSheet.assets[0]).toEqual({
        accountCode: '1100',
        accountName: '現金',
        amount: 500000
      })
      expect(balanceSheet.liabilities).toHaveLength(1)
      expect(balanceSheet.liabilities[0]).toEqual({
        accountCode: '2100',
        accountName: '買掛金',
        amount: 200000
      })
      expect(balanceSheet.equity).toHaveLength(1)
      expect(balanceSheet.equity[0]).toEqual({
        accountCode: '3100',
        accountName: '資本金',
        amount: 300000
      })
      expect(balanceSheet.totalAssets).toBe(500000)
      expect(balanceSheet.totalLiabilities).toBe(200000)
      expect(balanceSheet.totalEquity).toBe(300000)
    })

    it('正常系: 残高が0の勘定科目は含まれない', async () => {
      if (!testDb.prisma) throw new Error('Prisma not initialized')

      await testDb.prisma.account.createMany({
        data: [
          {
            accountCode: '1100',
            accountName: '現金',
            accountType: '資産',
            sumAccount: false,
            aggregationTarget: true,
            bsplDistinction: 'B'
          },
          {
            accountCode: '1200',
            accountName: '預金',
            accountType: '資産',
            sumAccount: false,
            aggregationTarget: true,
            bsplDistinction: 'B'
          }
        ]
      })

      await testDb.prisma.monthlyAccountBalance.createMany({
        data: [
          {
            fiscalYearMonth: '202401',
            accountCode: '1100',
            openingBalance: 0,
            debitAmount: 100000,
            creditAmount: 0,
            closingBalance: 100000
          },
          {
            fiscalYearMonth: '202401',
            accountCode: '1200',
            openingBalance: 0,
            debitAmount: 0,
            creditAmount: 0,
            closingBalance: 0
          }
        ]
      })

      const response = await app.inject({
        method: 'GET',
        url: '/statements/balance-sheet?asOfDate=2024-01-31'
      })

      expect(response.statusCode).toBe(200)
      const body = JSON.parse(response.body)
      const balanceSheet = body.data.balanceSheet
      expect(balanceSheet.assets).toHaveLength(1)
      expect(balanceSheet.assets[0].accountCode).toBe('1100')
    })

    it('異常系: 必須パラメータ asOfDate がない場合はエラー', async () => {
      const response = await app.inject({
        method: 'GET',
        url: '/statements/balance-sheet'
      })

      expect(response.statusCode).toBe(400)
    })
  })

  describe('GET /statements/income-statement', () => {
    it('正常系: 損益計算書を取得できる', async () => {
      if (!testDb.prisma) throw new Error('Prisma not initialized')

      // テストデータの準備
      await testDb.prisma.account.createMany({
        data: [
          {
            accountCode: '4100',
            accountName: '売上高',
            accountType: '収益',
            sumAccount: false,
            aggregationTarget: true,
            bsplDistinction: 'P'
          },
          {
            accountCode: '5100',
            accountName: '売上原価',
            accountType: '費用',
            sumAccount: false,
            aggregationTarget: true,
            bsplDistinction: 'P'
          },
          {
            accountCode: '6100',
            accountName: '販売管理費',
            accountType: '費用',
            sumAccount: false,
            aggregationTarget: true,
            bsplDistinction: 'P'
          }
        ]
      })

      // 3ヶ月分のデータを作成
      await testDb.prisma.monthlyAccountBalance.createMany({
        data: [
          // 2024年1月
          {
            fiscalYearMonth: '202401',
            accountCode: '4100',
            openingBalance: 0,
            debitAmount: 0,
            creditAmount: 1000000,
            closingBalance: 1000000
          },
          {
            fiscalYearMonth: '202401',
            accountCode: '5100',
            openingBalance: 0,
            debitAmount: 600000,
            creditAmount: 0,
            closingBalance: 600000
          },
          {
            fiscalYearMonth: '202401',
            accountCode: '6100',
            openingBalance: 0,
            debitAmount: 200000,
            creditAmount: 0,
            closingBalance: 200000
          },
          // 2024年2月
          {
            fiscalYearMonth: '202402',
            accountCode: '4100',
            openingBalance: 1000000,
            debitAmount: 0,
            creditAmount: 1200000,
            closingBalance: 2200000
          },
          {
            fiscalYearMonth: '202402',
            accountCode: '5100',
            openingBalance: 600000,
            debitAmount: 700000,
            creditAmount: 0,
            closingBalance: 1300000
          },
          {
            fiscalYearMonth: '202402',
            accountCode: '6100',
            openingBalance: 200000,
            debitAmount: 250000,
            creditAmount: 0,
            closingBalance: 450000
          },
          // 2024年3月
          {
            fiscalYearMonth: '202403',
            accountCode: '4100',
            openingBalance: 2200000,
            debitAmount: 0,
            creditAmount: 1500000,
            closingBalance: 3700000
          },
          {
            fiscalYearMonth: '202403',
            accountCode: '5100',
            openingBalance: 1300000,
            debitAmount: 900000,
            creditAmount: 0,
            closingBalance: 2200000
          },
          {
            fiscalYearMonth: '202403',
            accountCode: '6100',
            openingBalance: 450000,
            debitAmount: 300000,
            creditAmount: 0,
            closingBalance: 750000
          }
        ]
      })

      // API リクエスト（1月～3月の期間）
      const response = await app.inject({
        method: 'GET',
        url: '/statements/income-statement?fromDate=2024-01-01&toDate=2024-03-31'
      })

      // アサーション
      expect(response.statusCode).toBe(200)
      const body = JSON.parse(response.body)
      expect(body.success).toBe(true)
      expect(body.data.incomeStatement).toBeDefined()

      const incomeStatement = body.data.incomeStatement
      expect(incomeStatement.fromDate).toBe('2024-01-01')
      expect(incomeStatement.toDate).toBe('2024-03-31')

      // 収益の検証（1月1000000 + 2月1200000 + 3月1500000 = 3700000）
      expect(incomeStatement.revenues).toHaveLength(1)
      expect(incomeStatement.revenues[0]).toEqual({
        accountCode: '4100',
        accountName: '売上高',
        amount: 3700000
      })
      expect(incomeStatement.totalRevenues).toBe(3700000)

      // 費用の検証
      expect(incomeStatement.expenses).toHaveLength(2)
      // 売上原価: 600000 + 700000 + 900000 = 2200000
      const costOfSales = incomeStatement.expenses.find(
        (e: IncomeStatementItemDto) => e.accountCode === '5100'
      )
      expect(costOfSales).toEqual({
        accountCode: '5100',
        accountName: '売上原価',
        amount: 2200000
      })
      // 販売管理費: 200000 + 250000 + 300000 = 750000
      const sellingExpenses = incomeStatement.expenses.find(
        (e: IncomeStatementItemDto) => e.accountCode === '6100'
      )
      expect(sellingExpenses).toEqual({
        accountCode: '6100',
        accountName: '販売管理費',
        amount: 750000
      })
      expect(incomeStatement.totalExpenses).toBe(2950000)

      // 利益の検証（収益3700000 - 費用2950000 = 750000）
      expect(incomeStatement.netIncome).toBe(750000)
      expect(incomeStatement.grossProfit).toBe(750000)
      expect(incomeStatement.operatingIncome).toBe(750000)
    })

    it('正常系: 金額が0の勘定科目は含まれない', async () => {
      if (!testDb.prisma) throw new Error('Prisma not initialized')

      await testDb.prisma.account.createMany({
        data: [
          {
            accountCode: '4100',
            accountName: '売上高',
            accountType: '収益',
            sumAccount: false,
            aggregationTarget: true,
            bsplDistinction: 'P'
          },
          {
            accountCode: '4200',
            accountName: 'その他収益',
            accountType: '収益',
            sumAccount: false,
            aggregationTarget: true,
            bsplDistinction: 'P'
          }
        ]
      })

      await testDb.prisma.monthlyAccountBalance.createMany({
        data: [
          {
            fiscalYearMonth: '202401',
            accountCode: '4100',
            openingBalance: 0,
            debitAmount: 0,
            creditAmount: 500000,
            closingBalance: 500000
          },
          {
            fiscalYearMonth: '202401',
            accountCode: '4200',
            openingBalance: 0,
            debitAmount: 0,
            creditAmount: 0,
            closingBalance: 0
          }
        ]
      })

      const response = await app.inject({
        method: 'GET',
        url: '/statements/income-statement?fromDate=2024-01-01&toDate=2024-01-31'
      })

      expect(response.statusCode).toBe(200)
      const body = JSON.parse(response.body)
      const incomeStatement = body.data.incomeStatement
      expect(incomeStatement.revenues).toHaveLength(1)
      expect(incomeStatement.revenues[0].accountCode).toBe('4100')
    })

    it('異常系: 必須パラメータがない場合はエラー', async () => {
      const response1 = await app.inject({
        method: 'GET',
        url: '/statements/income-statement?fromDate=2024-01-01'
      })
      expect(response1.statusCode).toBe(400)

      const response2 = await app.inject({
        method: 'GET',
        url: '/statements/income-statement?toDate=2024-01-31'
      })
      expect(response2.statusCode).toBe(400)

      const response3 = await app.inject({
        method: 'GET',
        url: '/statements/income-statement'
      })
      expect(response3.statusCode).toBe(400)
    })
  })
})
