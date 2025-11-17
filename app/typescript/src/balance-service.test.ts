// src/balance-service.test.ts
import { describe, test, expect, beforeAll, afterAll, beforeEach } from 'vitest'
import { TestDatabase } from './test-setup/database'
import { BalanceService } from './balance-service'

describe('BalanceService', () => {
  let testDb: TestDatabase
  let balanceService: BalanceService

  beforeAll(async () => {
    testDb = new TestDatabase()
    await testDb.start()
  })

  afterAll(async () => {
    await testDb.stop()
  })

  beforeEach(async () => {
    await testDb.cleanup()

    if (!testDb.prisma) {
      throw new Error('Prisma client not initialized')
    }

    balanceService = new BalanceService(testDb.prisma)

    // テストデータ: 勘定科目マスタ
    await testDb.prisma.account.createMany({
      data: [
        {
          accountCode: '1110',
          accountName: '普通預金',
          accountKana: 'フツウヨキン',
          accountType: '資産',
          bsplDistinction: 'B'
        },
        {
          accountCode: '4100',
          accountName: '売上高',
          accountKana: 'ウリアゲダカ',
          accountType: '収益',
          bsplDistinction: 'P'
        }
      ]
    })
  })

  test('仕訳入力と同時に日次残高が更新される', async () => {
    if (!testDb.prisma) {
      throw new Error('Prisma client not initialized')
    }

    // 仕訳を登録
    await balanceService.createJournalWithBalance('J001', new Date('2024-01-15'), [
      {
        accountCode: '1110',
        debitOrCredit: '借',
        amount: 100000
      },
      {
        accountCode: '4100',
        debitOrCredit: '貸',
        amount: 100000
      }
    ])

    // 仕訳が登録されたことを確認
    const journal = await testDb.prisma.$queryRaw<
      Array<{
        仕訳伝票番号: string
      }>
    >`
      SELECT * FROM "仕訳"
      WHERE "仕訳伝票番号" = 'J001'
    `
    expect(journal).toHaveLength(1)

    // 仕訳明細が登録されたことを確認
    const details = await testDb.prisma.$queryRaw<
      Array<{
        仕訳行番号: number
      }>
    >`
      SELECT * FROM "仕訳明細"
      WHERE "仕訳伝票番号" = 'J001'
    `
    expect(details).toHaveLength(2)

    // 仕訳貸借明細が登録されたことを確認
    const items = await testDb.prisma.$queryRaw<
      Array<{
        勘定科目コード: string
        仕訳金額: number
      }>
    >`
      SELECT * FROM "仕訳貸借明細"
      WHERE "仕訳伝票番号" = 'J001'
      ORDER BY "仕訳行番号"
    `
    expect(items).toHaveLength(2)
    expect(items[0].勘定科目コード).toBe('1110')
    expect(Number(items[0].仕訳金額)).toBe(100000)

    // 日次残高が更新されたことを確認
    const dailyBalance = await testDb.prisma.$queryRaw<
      Array<{
        勘定科目コード: string
        借方金額: number
        貸方金額: number
      }>
    >`
      SELECT * FROM "日次勘定科目残高"
      WHERE "起票日" = '2024-01-15'::date
      ORDER BY "勘定科目コード"
    `
    expect(dailyBalance).toHaveLength(2)
    expect(dailyBalance[0].勘定科目コード).toBe('1110')
    expect(Number(dailyBalance[0].借方金額)).toBe(100000)
    expect(Number(dailyBalance[0].貸方金額)).toBe(0)
    expect(dailyBalance[1].勘定科目コード).toBe('4100')
    expect(Number(dailyBalance[1].借方金額)).toBe(0)
    expect(Number(dailyBalance[1].貸方金額)).toBe(100000)
  })

  test('同じ日に複数の仕訳を登録すると残高が累積される', async () => {
    if (!testDb.prisma) {
      throw new Error('Prisma client not initialized')
    }

    // 1回目の仕訳
    await balanceService.createJournalWithBalance('J001', new Date('2024-01-15'), [
      {
        accountCode: '1110',
        debitOrCredit: '借',
        amount: 100000
      },
      {
        accountCode: '4100',
        debitOrCredit: '貸',
        amount: 100000
      }
    ])

    // 2回目の仕訳（同じ日）
    await balanceService.createJournalWithBalance('J002', new Date('2024-01-15'), [
      {
        accountCode: '1110',
        debitOrCredit: '借',
        amount: 50000
      },
      {
        accountCode: '4100',
        debitOrCredit: '貸',
        amount: 50000
      }
    ])

    // 日次残高が累積されていることを確認
    const dailyBalance = await testDb.prisma.$queryRaw<
      Array<{
        勘定科目コード: string
        借方金額: number
        貸方金額: number
      }>
    >`
      SELECT * FROM "日次勘定科目残高"
      WHERE "起票日" = '2024-01-15'::date
      ORDER BY "勘定科目コード"
    `
    expect(dailyBalance).toHaveLength(2)
    expect(dailyBalance[0].勘定科目コード).toBe('1110')
    expect(Number(dailyBalance[0].借方金額)).toBe(150000) // 100000 + 50000
    expect(dailyBalance[1].勘定科目コード).toBe('4100')
    expect(Number(dailyBalance[1].貸方金額)).toBe(150000) // 100000 + 50000
  })

  test('部門別に残高が管理される', async () => {
    if (!testDb.prisma) {
      throw new Error('Prisma client not initialized')
    }

    // 部門001の仕訳
    await balanceService.createJournalWithBalance('J001', new Date('2024-01-15'), [
      {
        accountCode: '1110',
        departmentCode: '001',
        debitOrCredit: '借',
        amount: 100000
      },
      {
        accountCode: '4100',
        departmentCode: '001',
        debitOrCredit: '貸',
        amount: 100000
      }
    ])

    // 部門002の仕訳
    await balanceService.createJournalWithBalance('J002', new Date('2024-01-15'), [
      {
        accountCode: '1110',
        departmentCode: '002',
        debitOrCredit: '借',
        amount: 50000
      },
      {
        accountCode: '4100',
        departmentCode: '002',
        debitOrCredit: '貸',
        amount: 50000
      }
    ])

    // 部門別に残高が管理されていることを確認
    const dailyBalance = await testDb.prisma.$queryRaw<
      Array<{
        部門コード: string
        借方金額: number
      }>
    >`
      SELECT * FROM "日次勘定科目残高"
      WHERE "起票日" = '2024-01-15'::date
      AND "勘定科目コード" = '1110'
      ORDER BY "部門コード"
    `
    expect(dailyBalance).toHaveLength(2)
    expect(dailyBalance[0].部門コード).toBe('001')
    expect(Number(dailyBalance[0].借方金額)).toBe(100000)
    expect(dailyBalance[1].部門コード).toBe('002')
    expect(Number(dailyBalance[1].借方金額)).toBe(50000)
  })

  test('月次残高が正しく更新される', async () => {
    if (!testDb.prisma) {
      throw new Error('Prisma client not initialized')
    }

    // 日次残高を登録
    await testDb.prisma.$executeRaw`
      INSERT INTO "日次勘定科目残高" (
        "起票日", "勘定科目コード", "補助科目コード", "部門コード",
        "プロジェクトコード", "決算仕訳フラグ", "借方金額", "貸方金額"
      ) VALUES
      ('2024-01-15', '1110', '', '', '', 0, 100000, 0),
      ('2024-01-20', '1110', '', '', '', 0, 50000, 0),
      ('2024-01-25', '1110', '', '', '', 0, 0, 30000)
    `

    // 月次残高を更新
    await balanceService.updateMonthlyBalance('202401', '1110')

    // 月次残高が正しく計算されていることを確認
    const monthlyBalance = await testDb.prisma.$queryRaw<
      Array<{
        月初残高: number
        借方金額: number
        貸方金額: number
        月末残高: number
      }>
    >`
      SELECT * FROM "月次勘定科目残高"
      WHERE "会計年月" = '202401'
      AND "勘定科目コード" = '1110'
    `
    expect(monthlyBalance).toHaveLength(1)
    expect(Number(monthlyBalance[0].月初残高)).toBe(0)
    expect(Number(monthlyBalance[0].借方金額)).toBe(150000) // 100000 + 50000
    expect(Number(monthlyBalance[0].貸方金額)).toBe(30000)
    expect(Number(monthlyBalance[0].月末残高)).toBe(120000) // 0 + 150000 - 30000
  })
})
