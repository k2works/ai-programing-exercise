// src/account.test.ts
import { describe, test, expect, beforeAll, afterAll, beforeEach } from 'vitest'
import { TestDatabase } from './test-setup/database'

// テスト用の勘定科目データ
const accounts = [
  {
    id: 1,
    code: '1000',
    name: '現金',
    accountType: 'ASSET' as const,
    balance: 50000
  },
  {
    id: 2,
    code: '2000',
    name: '買掛金',
    accountType: 'LIABILITY' as const,
    balance: 30000
  },
  {
    id: 3,
    code: '3000',
    name: '資本金',
    accountType: 'EQUITY' as const,
    balance: 100000
  }
]

describe('勘定科目マスタ', () => {
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

  test('勘定科目を登録できる', async () => {
    if (!testDb.prisma) {
      throw new Error('Prisma client not initialized')
    }

    // 1. テストデータを作成
    const createdAccount = await testDb.prisma.account.create({
      data: accounts[0]
    })

    // 2. 取得したデータが期待通りか検証
    expect(createdAccount.code).toBe(accounts[0].code)
    expect(createdAccount.name).toBe(accounts[0].name)
    expect(createdAccount.accountType).toBe(accounts[0].accountType)
    expect(Number(createdAccount.balance)).toBe(accounts[0].balance)
  })

  test('すべての勘定科目を取得できる', async () => {
    if (!testDb.prisma) {
      throw new Error('Prisma client not initialized')
    }

    // 1. 複数の勘定科目を登録
    for (const account of accounts) {
      await testDb.prisma.account.create({ data: account })
    }

    // 2. すべての勘定科目を取得
    const result = await testDb.prisma.account.findMany({
      orderBy: { code: 'asc' }
    })

    // 3. 期待通りのデータが取得できるか検証
    expect(result).toHaveLength(3)
    expect(result[0].code).toBe('1000')
    expect(result[1].code).toBe('2000')
    expect(result[2].code).toBe('3000')
  })

  test('勘定科目コードで検索できる', async () => {
    if (!testDb.prisma) {
      throw new Error('Prisma client not initialized')
    }

    // 1. テストデータを登録
    await testDb.prisma.account.create({ data: accounts[0] })

    // 2. コードで検索
    const result = await testDb.prisma.account.findUnique({
      where: { code: '1000' }
    })

    // 3. 正しいデータが取得できるか検証
    expect(result).not.toBeNull()
    expect(result?.name).toBe('現金')
    expect(result?.accountType).toBe('ASSET')
  })

  test('勘定科目を更新できる', async () => {
    if (!testDb.prisma) {
      throw new Error('Prisma client not initialized')
    }

    // 1. データを登録
    const created = await testDb.prisma.account.create({ data: accounts[0] })

    // 2. データを更新
    const updated = await testDb.prisma.account.update({
      where: { id: created.id },
      data: { name: '現金及び預金', balance: 75000 }
    })

    // 3. 更新されたか検証
    expect(updated.name).toBe('現金及び預金')
    expect(Number(updated.balance)).toBe(75000)
    expect(updated.code).toBe('1000') // 変更していない項目は保持される
  })

  test('勘定科目を削除できる', async () => {
    if (!testDb.prisma) {
      throw new Error('Prisma client not initialized')
    }

    // 1. データを登録
    const created = await testDb.prisma.account.create({ data: accounts[0] })

    // 2. データを削除
    await testDb.prisma.account.delete({
      where: { id: created.id }
    })

    // 3. データが削除されたか検証
    const result = await testDb.prisma.account.findUnique({
      where: { id: created.id }
    })
    expect(result).toBeNull()
  })

  test('勘定科目種別でフィルタリングできる', async () => {
    if (!testDb.prisma) {
      throw new Error('Prisma client not initialized')
    }

    // 1. 複数の勘定科目を登録
    for (const account of accounts) {
      await testDb.prisma.account.create({ data: account })
    }

    // 2. 資産勘定のみを取得
    const assets = await testDb.prisma.account.findMany({
      where: { accountType: 'ASSET' }
    })

    // 3. 正しくフィルタリングされるか検証
    expect(assets).toHaveLength(1)
    expect(assets[0].name).toBe('現金')

    // 4. 負債勘定のみを取得
    const liabilities = await testDb.prisma.account.findMany({
      where: { accountType: 'LIABILITY' }
    })

    expect(liabilities).toHaveLength(1)
    expect(liabilities[0].name).toBe('買掛金')
  })
})
