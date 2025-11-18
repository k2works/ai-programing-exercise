// src/api/__tests__/journal-api.test.ts
import { describe, it, expect, beforeAll, afterAll, beforeEach } from 'vitest'
import { FastifyInstance } from 'fastify'
import { buildApp } from '../application'
import { TestDatabase } from '../../test-setup/database'

describe('Journal API 統合テスト', () => {
  let app: FastifyInstance
  let testDb: TestDatabase

  beforeAll(async () => {
    testDb = new TestDatabase()
    await testDb.start()
    app = await buildApp({ prisma: testDb.prisma })
  })

  afterAll(async () => {
    await app.close()
    await testDb.stop()
  })

  beforeEach(async () => {
    await testDb.cleanup()
  })

  describe('POST /journals - 仕訳作成', () => {
    it('正常系: 仕訳を作成できる', async () => {
      // テスト用の勘定科目を作成
      if (!testDb.prisma) throw new Error('Prisma not initialized')

      await testDb.prisma.account.createMany({
        data: [
          {
            accountCode: '1100',
            accountName: '現金',
            accountType: '資産',
            sumAccount: false,
            aggregationTarget: true
          },
          {
            accountCode: '4100',
            accountName: '売上',
            accountType: '収益',
            sumAccount: false,
            aggregationTarget: true
          }
        ]
      })

      const response = await app.inject({
        method: 'POST',
        url: '/journals',
        payload: {
          voucherNo: 'J2024001',
          journalDate: '2024-01-15',
          inputDate: '2024-01-15',
          settlementFlag: 0,
          singleFlag: 1,
          voucherType: 1,
          recurringFlag: 0,
          redSlipFlag: 0,
          details: [
            {
              lineNo: 1,
              lineSummary: '商品売上',
              items: [
                {
                  debitCredit: 'D',
                  currencyCode: 'JPY',
                  exchangeRate: 1.0,
                  accountCode: '1100',
                  amount: 110000,
                  baseAmount: 110000,
                  cashFlowFlag: 0
                },
                {
                  debitCredit: 'C',
                  currencyCode: 'JPY',
                  exchangeRate: 1.0,
                  accountCode: '4100',
                  amount: 110000,
                  baseAmount: 110000,
                  cashFlowFlag: 0
                }
              ]
            }
          ]
        }
      })

      expect(response.statusCode).toBe(201)
      const body = JSON.parse(response.body)
      expect(body.success).toBe(true)
      expect(body.data.journal.voucherNo).toBe('J2024001')
      expect(body.data.journal.details).toHaveLength(1)
      expect(body.data.journal.details[0].items).toHaveLength(2)
    })

    it('異常系: 重複した伝票番号はエラー', async () => {
      if (!testDb.prisma) throw new Error('Prisma not initialized')

      await testDb.prisma.account.create({
        data: {
          accountCode: '1100',
          accountName: '現金',
          accountType: '資産',
          sumAccount: false,
          aggregationTarget: true
        }
      })

      // 最初の仕訳を作成
      await testDb.prisma.journal.create({
        data: {
          voucherNo: 'J2024001',
          journalDate: new Date('2024-01-15'),
          inputDate: new Date('2024-01-15'),
          settlementFlag: 0,
          singleFlag: 1,
          voucherType: 1,
          recurringFlag: 0,
          redSlipFlag: 0,
          details: {
            create: [
              {
                lineNo: 1,
                lineSummary: 'テスト',
                items: {
                  create: [
                    {
                      debitCredit: 'D',
                      currencyCode: 'JPY',
                      exchangeRate: 1.0,
                      accountCode: '1100',
                      amount: 10000,
                      baseAmount: 10000,
                      cashFlowFlag: 0
                    },
                    {
                      debitCredit: 'C',
                      currencyCode: 'JPY',
                      exchangeRate: 1.0,
                      accountCode: '1100',
                      amount: 10000,
                      baseAmount: 10000,
                      cashFlowFlag: 0
                    }
                  ]
                }
              }
            ]
          }
        }
      })

      // 重複した伝票番号で作成を試みる
      const response = await app.inject({
        method: 'POST',
        url: '/journals',
        payload: {
          voucherNo: 'J2024001',
          journalDate: '2024-01-15',
          inputDate: '2024-01-15',
          voucherType: 1,
          details: [
            {
              lineNo: 1,
              lineSummary: 'テスト',
              items: [
                {
                  debitCredit: 'D',
                  currencyCode: 'JPY',
                  exchangeRate: 1.0,
                  accountCode: '1100',
                  amount: 10000,
                  baseAmount: 10000,
                  cashFlowFlag: 0
                },
                {
                  debitCredit: 'C',
                  currencyCode: 'JPY',
                  exchangeRate: 1.0,
                  accountCode: '1100',
                  amount: 10000,
                  baseAmount: 10000,
                  cashFlowFlag: 0
                }
              ]
            }
          ]
        }
      })

      expect(response.statusCode).toBe(409)
      const body = JSON.parse(response.body)
      expect(body.success).toBe(false)
      expect(body.code).toBe('JOURNAL_ALREADY_EXISTS')
    })

    it('異常系: 借方・貸方の合計が一致しない場合はエラー', async () => {
      if (!testDb.prisma) throw new Error('Prisma not initialized')

      await testDb.prisma.account.createMany({
        data: [
          {
            accountCode: '1100',
            accountName: '現金',
            accountType: '資産',
            sumAccount: false,
            aggregationTarget: true
          },
          {
            accountCode: '4100',
            accountName: '売上',
            accountType: '収益',
            sumAccount: false,
            aggregationTarget: true
          }
        ]
      })

      const response = await app.inject({
        method: 'POST',
        url: '/journals',
        payload: {
          voucherNo: 'J2024002',
          journalDate: '2024-01-15',
          inputDate: '2024-01-15',
          voucherType: 1,
          details: [
            {
              lineNo: 1,
              lineSummary: 'テスト',
              items: [
                {
                  debitCredit: 'D',
                  currencyCode: 'JPY',
                  exchangeRate: 1.0,
                  accountCode: '1100',
                  amount: 100000,
                  baseAmount: 100000,
                  cashFlowFlag: 0
                },
                {
                  debitCredit: 'C',
                  currencyCode: 'JPY',
                  exchangeRate: 1.0,
                  accountCode: '4100',
                  amount: 50000, // 借方と合計が一致しない
                  baseAmount: 50000,
                  cashFlowFlag: 0
                }
              ]
            }
          ]
        }
      })

      expect(response.statusCode).toBe(400)
      const body = JSON.parse(response.body)
      expect(body.success).toBe(false)
      expect(body.code).toBe('INVALID_JOURNAL_DATA')
    })
  })

  describe('GET /journals - 全仕訳取得', () => {
    it('正常系: 全仕訳を取得できる', async () => {
      if (!testDb.prisma) throw new Error('Prisma not initialized')

      await testDb.prisma.account.create({
        data: {
          accountCode: '1100',
          accountName: '現金',
          accountType: '資産',
          sumAccount: false,
          aggregationTarget: true
        }
      })

      // テスト用の仕訳を作成
      await testDb.prisma.journal.createMany({
        data: [
          {
            voucherNo: 'J2024001',
            journalDate: new Date('2024-01-15'),
            inputDate: new Date('2024-01-15'),
            settlementFlag: 0,
            singleFlag: 1,
            voucherType: 1,
            recurringFlag: 0,
            redSlipFlag: 0
          },
          {
            voucherNo: 'J2024002',
            journalDate: new Date('2024-01-16'),
            inputDate: new Date('2024-01-16'),
            settlementFlag: 0,
            singleFlag: 1,
            voucherType: 1,
            recurringFlag: 0,
            redSlipFlag: 0
          }
        ]
      })

      const response = await app.inject({
        method: 'GET',
        url: '/journals'
      })

      expect(response.statusCode).toBe(200)
      const body = JSON.parse(response.body)
      expect(body.success).toBe(true)
      expect(body.data.journals).toHaveLength(2)
    })
  })

  describe('GET /journals/:voucherNo - 伝票番号検索', () => {
    it('正常系: 指定した伝票番号の仕訳を取得できる', async () => {
      if (!testDb.prisma) throw new Error('Prisma not initialized')

      await testDb.prisma.account.create({
        data: {
          accountCode: '1100',
          accountName: '現金',
          accountType: '資産',
          sumAccount: false,
          aggregationTarget: true
        }
      })

      await testDb.prisma.journal.create({
        data: {
          voucherNo: 'J2024001',
          journalDate: new Date('2024-01-15'),
          inputDate: new Date('2024-01-15'),
          settlementFlag: 0,
          singleFlag: 1,
          voucherType: 1,
          recurringFlag: 0,
          redSlipFlag: 0,
          details: {
            create: [
              {
                lineNo: 1,
                lineSummary: 'テスト',
                items: {
                  create: [
                    {
                      debitCredit: 'D',
                      currencyCode: 'JPY',
                      exchangeRate: 1.0,
                      accountCode: '1100',
                      amount: 10000,
                      baseAmount: 10000,
                      cashFlowFlag: 0
                    },
                    {
                      debitCredit: 'C',
                      currencyCode: 'JPY',
                      exchangeRate: 1.0,
                      accountCode: '1100',
                      amount: 10000,
                      baseAmount: 10000,
                      cashFlowFlag: 0
                    }
                  ]
                }
              }
            ]
          }
        }
      })

      const response = await app.inject({
        method: 'GET',
        url: '/journals/J2024001'
      })

      expect(response.statusCode).toBe(200)
      const body = JSON.parse(response.body)
      expect(body.success).toBe(true)
      expect(body.data.journal.voucherNo).toBe('J2024001')
    })

    it('異常系: 存在しない伝票番号は404エラー', async () => {
      const response = await app.inject({
        method: 'GET',
        url: '/journals/NOTEXIST'
      })

      expect(response.statusCode).toBe(404)
      const body = JSON.parse(response.body)
      expect(body.success).toBe(false)
      expect(body.code).toBe('JOURNAL_NOT_FOUND')
    })
  })

  describe('DELETE /journals/:voucherNo - 仕訳削除', () => {
    it('正常系: 仕訳を削除できる', async () => {
      if (!testDb.prisma) throw new Error('Prisma not initialized')

      await testDb.prisma.account.create({
        data: {
          accountCode: '1100',
          accountName: '現金',
          accountType: '資産',
          sumAccount: false,
          aggregationTarget: true
        }
      })

      await testDb.prisma.journal.create({
        data: {
          voucherNo: 'J2024001',
          journalDate: new Date('2024-01-15'),
          inputDate: new Date('2024-01-15'),
          settlementFlag: 0,
          singleFlag: 1,
          voucherType: 1,
          recurringFlag: 0,
          redSlipFlag: 0,
          details: {
            create: [
              {
                lineNo: 1,
                lineSummary: 'テスト',
                items: {
                  create: [
                    {
                      debitCredit: 'D',
                      currencyCode: 'JPY',
                      exchangeRate: 1.0,
                      accountCode: '1100',
                      amount: 10000,
                      baseAmount: 10000,
                      cashFlowFlag: 0
                    },
                    {
                      debitCredit: 'C',
                      currencyCode: 'JPY',
                      exchangeRate: 1.0,
                      accountCode: '1100',
                      amount: 10000,
                      baseAmount: 10000,
                      cashFlowFlag: 0
                    }
                  ]
                }
              }
            ]
          }
        }
      })

      const response = await app.inject({
        method: 'DELETE',
        url: '/journals/J2024001'
      })

      expect(response.statusCode).toBe(200)
      const body = JSON.parse(response.body)
      expect(body.success).toBe(true)

      // 削除されたことを確認
      const deleted = await testDb.prisma.journal.findUnique({
        where: { voucherNo: 'J2024001' }
      })
      expect(deleted).toBeNull()
    })
  })
})
