// src/monthly-account-balance.test.ts
import { describe, test, expect, beforeAll, afterAll, beforeEach } from 'vitest'
import { TestDatabase } from './test-setup/database'

describe('月次勘定科目残高テーブル', () => {
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
  })

  test('月次残高レコードを登録できる', async () => {
    if (!testDb.prisma) {
      throw new Error('Prisma client not initialized')
    }

    // 月次残高の登録
    await testDb.prisma.$executeRaw`
      INSERT INTO "月次勘定科目残高" (
        "会計年月", "勘定科目コード", "補助科目コード", "部門コード",
        "プロジェクトコード", "決算仕訳フラグ",
        "月初残高", "借方金額", "貸方金額", "月末残高"
      ) VALUES
      ('202401', '1110', '', '', '', 0, 1000000, 500000, 300000, 1200000)
    `

    const result = await testDb.prisma.$queryRaw<
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

    expect(result).toHaveLength(1)
    expect(Number(result[0].月初残高)).toBe(1000000)
    expect(Number(result[0].借方金額)).toBe(500000)
    expect(Number(result[0].貸方金額)).toBe(300000)
    expect(Number(result[0].月末残高)).toBe(1200000)
  })

  test('複合キーで一意性が保たれる', async () => {
    if (!testDb.prisma) {
      throw new Error('Prisma client not initialized')
    }

    // 同じキーで登録
    await testDb.prisma.$executeRaw`
      INSERT INTO "月次勘定科目残高" (
        "会計年月", "勘定科目コード", "補助科目コード", "部門コード",
        "プロジェクトコード", "決算仕訳フラグ",
        "月初残高", "借方金額", "貸方金額", "月末残高"
      ) VALUES
      ('202401', '1110', '', '', '', 0, 1000000, 500000, 300000, 1200000)
    `

    // 同じキーで2回目の登録はエラー
    await expect(
      testDb.prisma.$executeRaw`
        INSERT INTO "月次勘定科目残高" (
          "会計年月", "勘定科目コード", "補助科目コード", "部門コード",
          "プロジェクトコード", "決算仕訳フラグ",
          "月初残高", "借方金額", "貸方金額", "月末残高"
        ) VALUES
        ('202401', '1110', '', '', '', 0, 500000, 200000, 100000, 600000)
      `
    ).rejects.toThrow()
  })

  test('部門別・プロジェクト別の月次残高を管理できる', async () => {
    if (!testDb.prisma) {
      throw new Error('Prisma client not initialized')
    }

    // 部門ごとの月次残高
    await testDb.prisma.$executeRaw`
      INSERT INTO "月次勘定科目残高" (
        "会計年月", "勘定科目コード", "補助科目コード", "部門コード",
        "プロジェクトコード", "決算仕訳フラグ",
        "月初残高", "借方金額", "貸方金額", "月末残高"
      ) VALUES
      ('202401', '4100', '', '001', '', 0, 0, 0, 3000000, 3000000),
      ('202401', '4100', '', '002', '', 0, 0, 0, 2000000, 2000000)
    `

    const result = await testDb.prisma.$queryRaw<
      Array<{
        部門コード: string
        売上合計: number
      }>
    >`
      SELECT "部門コード", SUM("月末残高") as 売上合計
      FROM "月次勘定科目残高"
      WHERE "勘定科目コード" = '4100'
      GROUP BY "部門コード"
      ORDER BY "部門コード"
    `

    expect(result).toHaveLength(2)
    expect(Number(result[0].売上合計)).toBe(3000000)
    expect(Number(result[1].売上合計)).toBe(2000000)
  })

  test('月次残高の整合性（月初残高 + 借方金額 - 貸方金額 = 月末残高）', async () => {
    if (!testDb.prisma) {
      throw new Error('Prisma client not initialized')
    }

    // 資産科目（借方残高）の例
    await testDb.prisma.$executeRaw`
      INSERT INTO "月次勘定科目残高" (
        "会計年月", "勘定科目コード", "補助科目コード", "部門コード",
        "プロジェクトコード", "決算仕訳フラグ",
        "月初残高", "借方金額", "貸方金額", "月末残高"
      ) VALUES
      ('202401', '1110', '', '', '', 0, 1000000, 500000, 300000, 1200000)
    `

    const result = await testDb.prisma.$queryRaw<
      Array<{
        月初残高: number
        借方金額: number
        貸方金額: number
        月末残高: number
        計算上の月末残高: number
      }>
    >`
      SELECT
        "月初残高",
        "借方金額",
        "貸方金額",
        "月末残高",
        ("月初残高" + "借方金額" - "貸方金額") as 計算上の月末残高
      FROM "月次勘定科目残高"
      WHERE "会計年月" = '202401'
      AND "勘定科目コード" = '1110'
    `

    expect(result).toHaveLength(1)
    expect(Number(result[0].月末残高)).toBe(Number(result[0].計算上の月末残高))
  })

  test('複数月の残高推移を管理できる', async () => {
    if (!testDb.prisma) {
      throw new Error('Prisma client not initialized')
    }

    // 3ヶ月分の残高データ
    await testDb.prisma.$executeRaw`
      INSERT INTO "月次勘定科目残高" (
        "会計年月", "勘定科目コード", "補助科目コード", "部門コード",
        "プロジェクトコード", "決算仕訳フラグ",
        "月初残高", "借方金額", "貸方金額", "月末残高"
      ) VALUES
      ('202401', '1110', '', '', '', 0, 1000000, 500000, 300000, 1200000),
      ('202402', '1110', '', '', '', 0, 1200000, 600000, 400000, 1400000),
      ('202403', '1110', '', '', '', 0, 1400000, 700000, 500000, 1600000)
    `

    const result = await testDb.prisma.$queryRaw<
      Array<{
        会計年月: string
        月末残高: number
      }>
    >`
      SELECT "会計年月", "月末残高"
      FROM "月次勘定科目残高"
      WHERE "勘定科目コード" = '1110'
      ORDER BY "会計年月"
    `

    expect(result).toHaveLength(3)
    expect(Number(result[0].月末残高)).toBe(1200000)
    expect(Number(result[1].月末残高)).toBe(1400000)
    expect(Number(result[2].月末残高)).toBe(1600000)

    // 前月の月末残高 = 当月の月初残高
    const nextMonth = await testDb.prisma.$queryRaw<
      Array<{
        月初残高: number
      }>
    >`
      SELECT "月初残高" FROM "月次勘定科目残高"
      WHERE "会計年月" = '202402' AND "勘定科目コード" = '1110'
    `

    expect(Number(result[0].月末残高)).toBe(Number(nextMonth[0].月初残高))
  })
})
