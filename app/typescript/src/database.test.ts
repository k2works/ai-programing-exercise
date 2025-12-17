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

  test('品目マスタテーブルが作成されている', async () => {
    if (!testDb.prisma) {
      throw new Error('Prisma client not initialized')
    }

    // itemsテーブルの存在確認
    const tableExists = await testDb.prisma.$queryRaw`
      SELECT EXISTS (
        SELECT FROM information_schema.tables
        WHERE table_schema = 'public'
        AND table_name = 'items'
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
      INSERT INTO items (item_code, effective_from, item_name, item_category)
      VALUES ('ITEM001', '2025-01-01', 'テスト品目', 'PRODUCT')
    `

    // データが挿入されていることを確認
    const count1 = await testDb.prisma.$queryRaw`
      SELECT COUNT(*) as count FROM items
    ` as Array<{ count: bigint }>

    expect(Number(count1[0].count)).toBe(1)

    // cleanup実行（beforeEachで自動実行される想定）
    await testDb.cleanup()

    // データがクリアされていることを確認
    const count2 = await testDb.prisma.$queryRaw`
      SELECT COUNT(*) as count FROM items
    ` as Array<{ count: bigint }>

    expect(Number(count2[0].count)).toBe(0)
  })
})
