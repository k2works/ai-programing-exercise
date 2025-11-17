// src/journal-entry.test.ts
import { describe, test, expect, beforeAll, afterAll, beforeEach } from 'vitest'
import { TestDatabase } from './test-setup/database'

describe('仕訳エントリの管理', () => {
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

  test('仕訳エントリを登録できる', async () => {
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
          accountCode: '2120',
          accountName: '仮受消費税',
          accountType: '負債',
          sumAccount: false,
          aggregationTarget: true
        }
      ]
    })

    // 基本的な仕訳エントリの作成
    const journalEntry = {
      entryNo: 'JE240001',
      entryDate: new Date('2024-01-15'),
      description: '現金売上',
      totalAmount: 110000,
      createdBy: 'user001'
    }

    const details = [
      {
        lineNo: 1,
        accountCode: '1100', // 現金
        debitAmount: 110000,
        creditAmount: 0,
        description: '商品売上による現金収入'
      },
      {
        lineNo: 2,
        accountCode: '4100', // 売上
        debitAmount: 0,
        creditAmount: 100000,
        description: '商品売上'
      },
      {
        lineNo: 3,
        accountCode: '2120', // 仮受消費税
        debitAmount: 0,
        creditAmount: 10000,
        description: '消費税'
      }
    ]

    // 仕訳エントリと明細行を一括で作成
    const created = await testDb.prisma.journalEntry.create({
      data: {
        ...journalEntry,
        details: {
          create: details
        }
      },
      include: { details: true }
    })

    // 作成されたデータの確認
    expect(created.entryNo).toBe('JE240001')
    expect(created.description).toBe('現金売上')
    expect(created.details).toHaveLength(3)

    // 借方・貸方の合計確認
    const debitTotal = created.details.reduce((sum, detail) => sum + Number(detail.debitAmount), 0)
    const creditTotal = created.details.reduce(
      (sum, detail) => sum + Number(detail.creditAmount),
      0
    )

    expect(debitTotal).toBe(creditTotal) // 複式簿記の原理
    expect(debitTotal).toBe(110000)
  })

  test('仕訳エントリを更新できる', async () => {
    if (!testDb.prisma) {
      throw new Error('Prisma client not initialized')
    }

    // テストデータの登録
    await testDb.prisma.journalEntry.create({
      data: {
        entryNo: 'JE240001',
        entryDate: new Date('2024-01-15'),
        description: '現金売上',
        totalAmount: 110000,
        createdBy: 'user001'
      }
    })

    // 既存の仕訳エントリを更新
    const updated = await testDb.prisma.journalEntry.update({
      where: { entryNo: 'JE240001' },
      data: {
        description: '現金売上（修正）',
        updatedBy: 'user002'
      }
    })

    expect(updated.description).toBe('現金売上（修正）')
    expect(updated.updatedBy).toBe('user002')
  })

  test('仕訳エントリを削除できる', async () => {
    if (!testDb.prisma) {
      throw new Error('Prisma client not initialized')
    }

    // テスト用の勘定科目を作成
    await testDb.prisma.account.create({
      data: {
        accountCode: '1000',
        accountName: '現金',
        accountType: '資産',
        sumAccount: false,
        aggregationTarget: true
      }
    })

    // テストデータの登録
    await testDb.prisma.journalEntry.create({
      data: {
        entryNo: 'JE240001',
        entryDate: new Date('2024-01-15'),
        description: '現金売上',
        totalAmount: 110000,
        createdBy: 'user001',
        details: {
          create: [
            {
              lineNo: 1,
              accountCode: '1000',
              debitAmount: 110000,
              creditAmount: 0,
              description: '現金'
            }
          ]
        }
      }
    })

    // 仕訳エントリの削除（明細行も連鎖削除される）
    await testDb.prisma.journalEntry.delete({
      where: { entryNo: 'JE240001' }
    })

    const result = await testDb.prisma.journalEntry.findUnique({
      where: { entryNo: 'JE240001' }
    })

    expect(result).toBeNull()
  })

  test('複雑な仕訳エントリ（売掛金回収）を登録できる', async () => {
    if (!testDb.prisma) {
      throw new Error('Prisma client not initialized')
    }

    // テスト用の勘定科目を作成
    await testDb.prisma.account.createMany({
      data: [
        {
          accountCode: '1200',
          accountName: '普通預金',
          accountType: '資産',
          sumAccount: false,
          aggregationTarget: true
        },
        {
          accountCode: '6200',
          accountName: '支払手数料',
          accountType: '費用',
          sumAccount: false,
          aggregationTarget: true
        },
        {
          accountCode: '1300',
          accountName: '売掛金',
          accountType: '資産',
          sumAccount: false,
          aggregationTarget: true
        }
      ]
    })

    const complexEntry = {
      entryNo: 'JE240002',
      entryDate: new Date('2024-01-20'),
      description: '売掛金回収と振込手数料',
      totalAmount: 105000,
      createdBy: 'user001'
    }

    const complexDetails = [
      {
        lineNo: 1,
        accountCode: '1200', // 普通預金
        debitAmount: 104500,
        creditAmount: 0,
        description: '売掛金回収（振込手数料差引後）'
      },
      {
        lineNo: 2,
        accountCode: '6200', // 支払手数料
        debitAmount: 500,
        creditAmount: 0,
        description: '振込手数料'
      },
      {
        lineNo: 3,
        accountCode: '1300', // 売掛金
        debitAmount: 0,
        creditAmount: 105000,
        description: '売掛金回収'
      }
    ]

    const created = await testDb.prisma.journalEntry.create({
      data: {
        ...complexEntry,
        details: {
          create: complexDetails
        }
      },
      include: { details: true }
    })

    expect(created.details).toHaveLength(3)

    // 借方・貸方の合計確認
    const debitTotal = created.details.reduce((sum, detail) => sum + Number(detail.debitAmount), 0)
    const creditTotal = created.details.reduce(
      (sum, detail) => sum + Number(detail.creditAmount),
      0
    )

    expect(debitTotal).toBe(creditTotal)
    expect(debitTotal).toBe(105000)
  })
})
