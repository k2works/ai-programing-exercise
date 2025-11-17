// src/ledger-and-trial-balance.test.ts
import { describe, test, expect, beforeAll, afterAll, beforeEach } from 'vitest'
import { TestDatabase } from './test-setup/database'

describe('総勘定元帳と試算表', () => {
  let testDb: TestDatabase

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

    // テストデータ: 日次勘定科目残高
    await testDb.prisma.$executeRaw`
      INSERT INTO "日次勘定科目残高" (
        "起票日", "勘定科目コード", "補助科目コード", "部門コード",
        "プロジェクトコード", "決算仕訳フラグ", "借方金額", "貸方金額"
      ) VALUES
      ('2024-01-10', '1110', '', '', '', 0, 1000000, 0),
      ('2024-01-15', '1110', '', '', '', 0, 500000, 0),
      ('2024-01-20', '1110', '', '', '', 0, 0, 300000),
      ('2024-01-10', '4100', '', '', '', 0, 0, 1000000),
      ('2024-01-15', '4100', '', '', '', 0, 0, 500000)
    `
  })

  test('総勘定元帳ビューが正しく生成される', async () => {
    if (!testDb.prisma) {
      throw new Error('Prisma client not initialized')
    }

    const ledger = await testDb.prisma.$queryRaw<
      Array<{
        entry_date: Date
        account_code: string
        account_name: string
        debit_amount: number
        credit_amount: number
        balance: number
      }>
    >`
      SELECT * FROM "総勘定元帳"
      WHERE account_code = '1110'
      ORDER BY entry_date
    `

    expect(ledger).toHaveLength(3)
    expect(ledger[0].account_code).toBe('1110')
    expect(ledger[0].account_name).toBe('普通預金')
    expect(Number(ledger[0].debit_amount)).toBe(1000000)
    expect(Number(ledger[0].balance)).toBe(1000000)

    expect(Number(ledger[1].debit_amount)).toBe(500000)
    expect(Number(ledger[1].balance)).toBe(1500000)

    expect(Number(ledger[2].credit_amount)).toBe(300000)
    expect(Number(ledger[2].balance)).toBe(1200000)
  })

  test('総勘定元帳で累積残高が正しく計算される', async () => {
    if (!testDb.prisma) {
      throw new Error('Prisma client not initialized')
    }

    const ledger = await testDb.prisma.$queryRaw<
      Array<{
        entry_date: Date
        debit_amount: number
        credit_amount: number
        balance: number
      }>
    >`
      SELECT * FROM "総勘定元帳"
      WHERE account_code = '1110'
      ORDER BY entry_date
    `

    // 1行目: 1,000,000 - 0 = 1,000,000
    expect(Number(ledger[0].balance)).toBe(1000000)

    // 2行目: 1,000,000 + 500,000 - 0 = 1,500,000
    expect(Number(ledger[1].balance)).toBe(1500000)

    // 3行目: 1,500,000 + 0 - 300,000 = 1,200,000
    expect(Number(ledger[2].balance)).toBe(1200000)
  })

  test('試算表ビューが正しく生成される', async () => {
    if (!testDb.prisma) {
      throw new Error('Prisma client not initialized')
    }

    const trialBalance = await testDb.prisma.$queryRaw<
      Array<{
        account_code: string
        account_name: string
        bspl_type: string
        transaction_type: string
        debit_total: number
        credit_total: number
        balance: number
      }>
    >`
      SELECT * FROM "試算表"
      ORDER BY account_code
    `

    expect(trialBalance).toHaveLength(2)

    // 普通預金（借方残高）
    const bankAccount = trialBalance[0]
    expect(bankAccount.account_code).toBe('1110')
    expect(bankAccount.account_name).toBe('普通預金')
    expect(Number(bankAccount.debit_total)).toBe(1500000) // 1,000,000 + 500,000
    expect(Number(bankAccount.credit_total)).toBe(300000)
    expect(Number(bankAccount.balance)).toBe(1200000) // 1,500,000 - 300,000

    // 売上高（貸方残高）
    const salesAccount = trialBalance[1]
    expect(salesAccount.account_code).toBe('4100')
    expect(salesAccount.account_name).toBe('売上高')
    expect(Number(salesAccount.debit_total)).toBe(0)
    expect(Number(salesAccount.credit_total)).toBe(1500000) // 1,000,000 + 500,000
    expect(Number(salesAccount.balance)).toBe(-1500000) // 0 - 1,500,000 (貸方なのでマイナス)
  })

  test('試算表で貸借合計が一致する', async () => {
    if (!testDb.prisma) {
      throw new Error('Prisma client not initialized')
    }

    const trialBalance = await testDb.prisma.$queryRaw<
      Array<{
        total_debit: number
        total_credit: number
      }>
    >`
      SELECT
        SUM(debit_total) as total_debit,
        SUM(credit_total) as total_credit
      FROM "試算表"
    `

    expect(trialBalance).toHaveLength(1)
    // 借方合計: 普通預金 1,500,000
    expect(Number(trialBalance[0].total_debit)).toBe(1500000)
    // 貸方合計: 普通預金 300,000 + 売上高 1,500,000 = 1,800,000
    expect(Number(trialBalance[0].total_credit)).toBe(1800000)
  })

  test('部門別の総勘定元帳が作成できる', async () => {
    if (!testDb.prisma) {
      throw new Error('Prisma client not initialized')
    }

    // 部門別データを追加
    await testDb.prisma.$executeRaw`
      INSERT INTO "日次勘定科目残高" (
        "起票日", "勘定科目コード", "補助科目コード", "部門コード",
        "プロジェクトコード", "決算仕訳フラグ", "借方金額", "貸方金額"
      ) VALUES
      ('2024-01-10', '4100', '', '001', '', 0, 0, 300000),
      ('2024-01-10', '4100', '', '002', '', 0, 0, 200000)
    `

    const ledger = await testDb.prisma.$queryRaw<
      Array<{
        department_code: string
        debit_amount: number
        credit_amount: number
      }>
    >`
      SELECT * FROM "総勘定元帳"
      WHERE account_code = '4100'
      AND department_code != ''
      ORDER BY department_code
    `

    expect(ledger).toHaveLength(2)
    expect(ledger[0].department_code).toBe('001')
    expect(Number(ledger[0].credit_amount)).toBe(300000)
    expect(ledger[1].department_code).toBe('002')
    expect(Number(ledger[1].credit_amount)).toBe(200000)
  })
})
