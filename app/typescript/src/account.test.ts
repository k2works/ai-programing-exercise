// src/account.test.ts
import { describe, test, expect, beforeAll, afterAll, beforeEach } from 'vitest'
import { TestDatabase } from './test-setup/database'

// テスト用の勘定科目データ
const accounts = [
  {
    accountCode: '1000',
    accountName: '現金',
    accountType: '資産',
    sumAccount: false,
    aggregationTarget: true
  },
  {
    accountCode: '2000',
    accountName: '買掛金',
    accountType: '負債',
    sumAccount: false,
    aggregationTarget: true
  },
  {
    accountCode: '3000',
    accountName: '資本金',
    accountType: '純資産',
    sumAccount: false,
    aggregationTarget: true
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
    expect(createdAccount.accountCode).toBe(accounts[0].accountCode)
    expect(createdAccount.accountName).toBe(accounts[0].accountName)
    expect(createdAccount.accountType).toBe(accounts[0].accountType)
    expect(createdAccount.sumAccount).toBe(accounts[0].sumAccount)
    expect(createdAccount.aggregationTarget).toBe(accounts[0].aggregationTarget)
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
      orderBy: { accountCode: 'asc' }
    })

    // 3. 期待通りのデータが取得できるか検証
    expect(result).toHaveLength(3)
    expect(result[0].accountCode).toBe('1000')
    expect(result[1].accountCode).toBe('2000')
    expect(result[2].accountCode).toBe('3000')
  })

  test('勘定科目コードで検索できる', async () => {
    if (!testDb.prisma) {
      throw new Error('Prisma client not initialized')
    }

    // 1. テストデータを登録
    await testDb.prisma.account.create({ data: accounts[0] })

    // 2. コードで検索
    const result = await testDb.prisma.account.findUnique({
      where: { accountCode: '1000' }
    })

    // 3. 正しいデータが取得できるか検証
    expect(result).not.toBeNull()
    expect(result?.accountName).toBe('現金')
    expect(result?.accountType).toBe('資産')
  })

  test('勘定科目を更新できる', async () => {
    if (!testDb.prisma) {
      throw new Error('Prisma client not initialized')
    }

    // 1. データを登録
    const created = await testDb.prisma.account.create({ data: accounts[0] })

    // 2. データを更新
    const updated = await testDb.prisma.account.update({
      where: { accountCode: created.accountCode },
      data: { accountName: '現金及び預金', displayOrder: 100 }
    })

    // 3. 更新されたか検証
    expect(updated.accountName).toBe('現金及び預金')
    expect(updated.displayOrder).toBe(100)
    expect(updated.accountCode).toBe('1000') // 変更していない項目は保持される
  })

  test('勘定科目を削除できる', async () => {
    if (!testDb.prisma) {
      throw new Error('Prisma client not initialized')
    }

    // 1. データを登録
    const created = await testDb.prisma.account.create({ data: accounts[0] })

    // 2. データを削除
    await testDb.prisma.account.delete({
      where: { accountCode: created.accountCode }
    })

    // 3. データが削除されたか検証
    const result = await testDb.prisma.account.findUnique({
      where: { accountCode: created.accountCode }
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
      where: { accountType: '資産' }
    })

    // 3. 正しくフィルタリングされるか検証
    expect(assets).toHaveLength(1)
    expect(assets[0].accountName).toBe('現金')

    // 4. 負債勘定のみを取得
    const liabilities = await testDb.prisma.account.findMany({
      where: { accountType: '負債' }
    })

    expect(liabilities).toHaveLength(1)
    expect(liabilities[0].accountName).toBe('買掛金')
  })
})
