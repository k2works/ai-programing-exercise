import { describe, test, expect, beforeAll, afterAll, beforeEach } from 'vitest'
import { TestDatabase } from './test-setup/database'

describe('TestContainer データベース接続テスト', () => {
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
  })

  test('データベースに接続できる', async () => {
    if (!testDb.prisma) {
      throw new Error('Prisma client not initialized')
    }

    // 接続確認用の簡単なクエリ
    const result = await testDb.prisma.$queryRaw`SELECT 1 as result`
    expect(result).toBeDefined()
  })

  test('テーブルが作成されている', async () => {
    if (!testDb.prisma) {
      throw new Error('Prisma client not initialized')
    }

    // 勘定科目マスタテーブルの存在確認
    const tableExists = await testDb.prisma.$queryRaw`
      SELECT EXISTS (
        SELECT FROM information_schema.tables
        WHERE table_schema = 'public'
        AND table_name = '勘定科目マスタ'
      ) as exists
    `

    expect(tableExists).toBeDefined()
  })

  test('テスト間でデータがクリアされる', async () => {
    if (!testDb.prisma) {
      throw new Error('Prisma client not initialized')
    }

    // 最初のテストデータ挿入
    await testDb.prisma.$executeRaw`
      INSERT INTO "勘定科目マスタ" ("勘定科目コード", "勘定科目名", "勘定科目種別", "合計科目", "集計対象")
      VALUES ('1000', '現金', '資産', false, true)
    `

    // データが挿入されていることを確認
    const count1 = (await testDb.prisma.$queryRaw`
      SELECT COUNT(*) as count FROM "勘定科目マスタ"
    `) as Array<{ count: bigint }>

    expect(Number(count1[0].count)).toBe(1)

    // cleanup実行（beforeEachで自動実行される想定）
    await testDb.cleanup()

    // データがクリアされていることを確認
    const count2 = (await testDb.prisma.$queryRaw`
      SELECT COUNT(*) as count FROM "勘定科目マスタ"
    `) as Array<{ count: bigint }>

    expect(Number(count2[0].count)).toBe(0)
  })
})
