// src/daily-account-balance.test.ts
import { describe, test, expect, beforeAll, afterAll, beforeEach } from 'vitest'
import { TestDatabase } from './test-setup/database'

describe('日次勘定科目残高テーブル', () => {
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

  test('日次残高レコードを登録できる', async () => {
    if (!testDb.prisma) {
      throw new Error('Prisma client not initialized')
    }

    // 日次残高の登録
    await testDb.prisma.$executeRaw`
      INSERT INTO "日次勘定科目残高" (
        "起票日", "勘定科目コード", "補助科目コード", "部門コード",
        "プロジェクトコード", "決算仕訳フラグ", "借方金額", "貸方金額"
      ) VALUES
      ('2024-01-15', '1110', '', '', '', 0, 100000, 0)
    `

    const result = await testDb.prisma.$queryRaw<
      Array<{
        借方金額: number
        貸方金額: number
      }>
    >`
      SELECT * FROM "日次勘定科目残高"
      WHERE "起票日" = '2024-01-15'::date
      AND "勘定科目コード" = '1110'
    `

    expect(result).toHaveLength(1)
    expect(Number(result[0].借方金額)).toBe(100000)
    expect(Number(result[0].貸方金額)).toBe(0)
  })

  test('複合キーで一意性が保たれる', async () => {
    if (!testDb.prisma) {
      throw new Error('Prisma client not initialized')
    }

    // 同じキーで登録
    await testDb.prisma.$executeRaw`
      INSERT INTO "日次勘定科目残高" (
        "起票日", "勘定科目コード", "補助科目コード", "部門コード",
        "プロジェクトコード", "決算仕訳フラグ", "借方金額", "貸方金額"
      ) VALUES
      ('2024-01-15', '1110', '', '', '', 0, 100000, 0)
    `

    // 同じキーで2回目の登録はエラー
    await expect(
      testDb.prisma.$executeRaw`
        INSERT INTO "日次勘定科目残高" (
          "起票日", "勘定科目コード", "補助科目コード", "部門コード",
          "プロジェクトコード", "決算仕訳フラグ", "借方金額", "貸方金額"
        ) VALUES
        ('2024-01-15', '1110', '', '', '', 0, 50000, 0)
      `
    ).rejects.toThrow()
  })

  test('部門別・プロジェクト別の残高を管理できる', async () => {
    if (!testDb.prisma) {
      throw new Error('Prisma client not initialized')
    }

    // 部門ごとの残高
    await testDb.prisma.$executeRaw`
      INSERT INTO "日次勘定科目残高" (
        "起票日", "勘定科目コード", "補助科目コード", "部門コード",
        "プロジェクトコード", "決算仕訳フラグ", "借方金額", "貸方金額"
      ) VALUES
      ('2024-01-15', '4100', '', '001', '', 0, 0, 300000),
      ('2024-01-15', '4100', '', '002', '', 0, 0, 200000)
    `

    const result = await testDb.prisma.$queryRaw<
      Array<{
        部門コード: string
        売上合計: number
      }>
    >`
      SELECT "部門コード", SUM("貸方金額") as 売上合計
      FROM "日次勘定科目残高"
      WHERE "勘定科目コード" = '4100'
      GROUP BY "部門コード"
      ORDER BY "部門コード"
    `

    expect(result).toHaveLength(2)
    expect(Number(result[0].売上合計)).toBe(300000)
    expect(Number(result[1].売上合計)).toBe(200000)
  })

  test('決算仕訳と通常仕訳を分けて管理できる', async () => {
    if (!testDb.prisma) {
      throw new Error('Prisma client not initialized')
    }

    // 通常仕訳と決算仕訳
    await testDb.prisma.$executeRaw`
      INSERT INTO "日次勘定科目残高" (
        "起票日", "勘定科目コード", "補助科目コード", "部門コード",
        "プロジェクトコード", "決算仕訳フラグ", "借方金額", "貸方金額"
      ) VALUES
      ('2024-12-31', '4100', '', '', '', 0, 0, 1000000),
      ('2024-12-31', '4100', '', '', '', 1, 500000, 0)
    `

    // 通常仕訳のみ
    const normal = await testDb.prisma.$queryRaw<
      Array<{
        残高: number
      }>
    >`
      SELECT SUM("貸方金額") - SUM("借方金額") as 残高
      FROM "日次勘定科目残高"
      WHERE "勘定科目コード" = '4100'
      AND "決算仕訳フラグ" = 0
    `

    expect(Number(normal[0].残高)).toBe(1000000)

    // 決算仕訳のみ
    const settlement = await testDb.prisma.$queryRaw<
      Array<{
        残高: number
      }>
    >`
      SELECT SUM("貸方金額") - SUM("借方金額") as 残高
      FROM "日次勘定科目残高"
      WHERE "勘定科目コード" = '4100'
      AND "決算仕訳フラグ" = 1
    `

    expect(Number(settlement[0].残高)).toBe(-500000)
  })
})
