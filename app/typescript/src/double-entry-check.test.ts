// src/double-entry-check.test.ts
import { describe, test, expect, beforeAll, afterAll, beforeEach } from 'vitest'
import { TestDatabase } from './test-setup/database'

describe('複式簿記の原理チェック', () => {
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

  test('正しい仕訳は複式簿記チェックをパスする', async () => {
    if (!testDb.prisma) {
      throw new Error('Prisma client not initialized')
    }

    // テスト用の勘定科目を作成
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

    // 正しい仕訳を作成（借方 = 貸方）
    await testDb.prisma.journalEntry.create({
      data: {
        entryNo: 'JE240001',
        entryDate: new Date('2024-01-15'),
        description: '現金売上',
        totalAmount: 100000,
        createdBy: 'user001',
        details: {
          create: [
            {
              lineNo: 1,
              accountCode: '1100',
              debitAmount: 100000,
              creditAmount: 0,
              description: '現金'
            },
            {
              lineNo: 2,
              accountCode: '4100',
              debitAmount: 0,
              creditAmount: 100000,
              description: '売上'
            }
          ]
        }
      }
    })

    // 仕訳残高チェックビューから差額を確認
    const result = await testDb.prisma.$queryRaw<
      Array<{
        伝票番号: string
        // eslint-disable-next-line @typescript-eslint/no-explicit-any
        借方合計: any
        // eslint-disable-next-line @typescript-eslint/no-explicit-any
        貸方合計: any
        // eslint-disable-next-line @typescript-eslint/no-explicit-any
        差額: any
      }>
    >`
      SELECT * FROM "仕訳残高チェック"
      WHERE "伝票番号" = 'JE240001'
    `

    expect(result).toHaveLength(1)
    expect(Number(result[0].差額)).toBe(0) // 借方 = 貸方
  })

  test('不正な仕訳は複式簿記チェックで検出される', async () => {
    if (!testDb.prisma) {
      throw new Error('Prisma client not initialized')
    }

    // テスト用の勘定科目を作成
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

    // 不正な仕訳を作成（借方 ≠ 貸方）
    await testDb.prisma.journalEntry.create({
      data: {
        entryNo: 'JE240002',
        entryDate: new Date('2024-01-15'),
        description: '不正な仕訳',
        totalAmount: 100000,
        createdBy: 'user001',
        details: {
          create: [
            {
              lineNo: 1,
              accountCode: '1100',
              debitAmount: 100000,
              creditAmount: 0,
              description: '現金'
            },
            {
              lineNo: 2,
              accountCode: '4100',
              debitAmount: 0,
              creditAmount: 90000, // 意図的に不一致
              description: '売上'
            }
          ]
        }
      }
    })

    // 複式簿記チェック関数で不整合を検出
    const violations = await testDb.prisma.$queryRaw<
      Array<{
        不整合伝票番号: string
        // eslint-disable-next-line @typescript-eslint/no-explicit-any
        差額: any
      }>
    >`
      SELECT * FROM 複式簿記チェック()
      WHERE "不整合伝票番号" = 'JE240002'
    `

    expect(violations).toHaveLength(1)
    expect(Number(violations[0].差額)).toBe(10000) // 借方 - 貸方 = 10000
  })

  test('複数の正しい仕訳はすべてチェックをパスする', async () => {
    if (!testDb.prisma) {
      throw new Error('Prisma client not initialized')
    }

    // テスト用の勘定科目を作成
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
        },
        {
          accountCode: '5100',
          accountName: '仕入',
          accountType: '費用',
          sumAccount: false,
          aggregationTarget: true
        }
      ]
    })

    // 複数の正しい仕訳を作成
    await testDb.prisma.journalEntry.createMany({
      data: [
        {
          entryNo: 'JE240003',
          entryDate: new Date('2024-01-15'),
          description: '仕訳1',
          totalAmount: 100000,
          createdBy: 'user001'
        },
        {
          entryNo: 'JE240004',
          entryDate: new Date('2024-01-16'),
          description: '仕訳2',
          totalAmount: 200000,
          createdBy: 'user001'
        }
      ]
    })

    await testDb.prisma.journalEntryDetail.createMany({
      data: [
        // JE240003
        {
          entryNo: 'JE240003',
          lineNo: 1,
          accountCode: '1100',
          debitAmount: 100000,
          creditAmount: 0,
          description: '現金'
        },
        {
          entryNo: 'JE240003',
          lineNo: 2,
          accountCode: '4100',
          debitAmount: 0,
          creditAmount: 100000,
          description: '売上'
        },
        // JE240004
        {
          entryNo: 'JE240004',
          lineNo: 1,
          accountCode: '5100',
          debitAmount: 200000,
          creditAmount: 0,
          description: '仕入'
        },
        {
          entryNo: 'JE240004',
          lineNo: 2,
          accountCode: '1100',
          debitAmount: 0,
          creditAmount: 200000,
          description: '現金'
        }
      ]
    })

    // 複式簿記チェック関数で不整合を検出（不整合がないことを確認）
    const violations = await testDb.prisma.$queryRaw<
      Array<{
        不整合伝票番号: string
        差額: number
      }>
    >`
      SELECT * FROM 複式簿記チェック()
      WHERE "不整合伝票番号" IN ('JE240003', 'JE240004')
    `

    expect(violations).toHaveLength(0) // 不整合なし
  })

  test('仕訳残高チェックビューで全仕訳の残高を確認できる', async () => {
    if (!testDb.prisma) {
      throw new Error('Prisma client not initialized')
    }

    // テスト用の勘定科目を作成
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

    // 仕訳を作成
    await testDb.prisma.journalEntry.create({
      data: {
        entryNo: 'JE240005',
        entryDate: new Date('2024-01-15'),
        description: '現金売上',
        totalAmount: 150000,
        createdBy: 'user001',
        details: {
          create: [
            {
              lineNo: 1,
              accountCode: '1100',
              debitAmount: 150000,
              creditAmount: 0,
              description: '現金'
            },
            {
              lineNo: 2,
              accountCode: '4100',
              debitAmount: 0,
              creditAmount: 150000,
              description: '売上'
            }
          ]
        }
      }
    })

    // 仕訳残高チェックビューから全データを取得
    const allBalances = await testDb.prisma.$queryRaw<
      Array<{
        伝票番号: string
        // eslint-disable-next-line @typescript-eslint/no-explicit-any
        借方合計: any
        // eslint-disable-next-line @typescript-eslint/no-explicit-any
        貸方合計: any
        // eslint-disable-next-line @typescript-eslint/no-explicit-any
        差額: any
      }>
    >`
      SELECT * FROM "仕訳残高チェック"
    `

    expect(allBalances.length).toBeGreaterThan(0)

    // すべての仕訳で借方合計と貸方合計が一致していることを確認
    const hasBalance = allBalances.every((row) => Number(row.差額) === 0)
    expect(hasBalance).toBe(true)
  })
})
