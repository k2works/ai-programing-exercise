// src/account-structure.test.ts
import { describe, test, expect, beforeAll, afterAll, beforeEach } from 'vitest'
import { TestDatabase } from './test-setup/database'

describe('勘定科目構成マスタ', () => {
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

  test('勘定科目の階層構造を登録できる', async () => {
    if (!testDb.prisma) {
      throw new Error('Prisma client not initialized')
    }

    // 勘定科目を作成
    await testDb.prisma.account.createMany({
      data: [
        {
          accountCode: '11',
          accountName: '資産の部',
          accountType: '資産',
          sumAccount: true,
          aggregationTarget: true
        },
        {
          accountCode: '11000',
          accountName: '流動資産',
          accountType: '資産',
          sumAccount: true,
          aggregationTarget: true
        },
        {
          accountCode: '11190',
          accountName: '現金及び預金',
          accountType: '資産',
          sumAccount: true,
          aggregationTarget: true
        },
        {
          accountCode: '11110',
          accountName: '現金',
          accountType: '資産',
          sumAccount: false,
          aggregationTarget: true
        },
        {
          accountCode: '11120',
          accountName: '当座預金',
          accountType: '資産',
          sumAccount: false,
          aggregationTarget: true
        }
      ]
    })

    // 階層構造を作成
    const structures = await testDb.prisma.accountStructure.createMany({
      data: [
        { accountCode: '11', accountPath: '11' },
        { accountCode: '11000', accountPath: '11^11000' },
        { accountCode: '11190', accountPath: '11^11000^11190' },
        { accountCode: '11110', accountPath: '11^11000^11190^11110' },
        { accountCode: '11120', accountPath: '11^11000^11190^11120' }
      ]
    })

    expect(structures.count).toBe(5)
  })

  test('勘定科目パスから親科目を取得できる', async () => {
    if (!testDb.prisma) {
      throw new Error('Prisma client not initialized')
    }

    // 勘定科目と構成を作成
    await testDb.prisma.account.create({
      data: {
        accountCode: '11110',
        accountName: '現金',
        accountType: '資産',
        sumAccount: false,
        aggregationTarget: true,
        structure: {
          create: {
            accountPath: '11^11000^11190^11110'
          }
        }
      },
      include: { structure: true }
    })

    // パスから親科目コードを抽出
    const account = await testDb.prisma.accountStructure.findUnique({
      where: { accountCode: '11110' }
    })

    expect(account?.accountPath).toBe('11^11000^11190^11110')

    // パスを分割して親科目を取得
    const pathParts = account?.accountPath.split('^') || []
    expect(pathParts).toHaveLength(4)
    expect(pathParts[0]).toBe('11') // 資産の部
    expect(pathParts[1]).toBe('11000') // 流動資産
    expect(pathParts[2]).toBe('11190') // 現金及び預金
    expect(pathParts[3]).toBe('11110') // 現金
  })

  test('特定の親科目配下の子科目を取得できる', async () => {
    if (!testDb.prisma) {
      throw new Error('Prisma client not initialized')
    }

    // テストデータの準備
    await testDb.prisma.account.createMany({
      data: [
        {
          accountCode: '11190',
          accountName: '現金及び預金',
          accountType: '資産',
          sumAccount: true,
          aggregationTarget: true
        },
        {
          accountCode: '11110',
          accountName: '現金',
          accountType: '資産',
          sumAccount: false,
          aggregationTarget: true
        },
        {
          accountCode: '11120',
          accountName: '当座預金',
          accountType: '資産',
          sumAccount: false,
          aggregationTarget: true
        },
        {
          accountCode: '11130',
          accountName: '普通預金',
          accountType: '資産',
          sumAccount: false,
          aggregationTarget: true
        }
      ]
    })

    await testDb.prisma.accountStructure.createMany({
      data: [
        { accountCode: '11190', accountPath: '11^11000^11190' },
        { accountCode: '11110', accountPath: '11^11000^11190^11110' },
        { accountCode: '11120', accountPath: '11^11000^11190^11120' },
        { accountCode: '11130', accountPath: '11^11000^11190^11130' }
      ]
    })

    // 現金及び預金（11190）配下の科目を検索
    const childAccounts = await testDb.prisma.accountStructure.findMany({
      where: {
        accountPath: {
          contains: '11190'
        },
        accountCode: {
          not: '11190' // 親科目自身は除外
        }
      },
      include: {
        account: true
      }
    })

    expect(childAccounts).toHaveLength(3)
    expect(childAccounts.map((c) => c.account.accountName)).toContain('現金')
    expect(childAccounts.map((c) => c.account.accountName)).toContain('当座預金')
    expect(childAccounts.map((c) => c.account.accountName)).toContain('普通預金')
  })
})
