// src/auto-journal.test.ts
import { describe, test, expect, beforeAll, afterAll, beforeEach } from 'vitest'
import { TestDatabase } from './test-setup/database'

describe('自動仕訳の管理', () => {
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

  test('日付管理方式: 最終処理日以降のデータを抽出できる', async () => {
    if (!testDb.prisma) {
      throw new Error('Prisma client not initialized')
    }

    // 自動仕訳管理レコードを作成
    const management = await testDb.prisma.autoJournalManagement.create({
      data: {
        sourceTable: '売上データ',
        lastProcessedAt: new Date('2024-01-15T00:00:00Z')
      }
    })

    expect(management.sourceTable).toBe('売上データ')
    expect(management.lastProcessedAt).toEqual(new Date('2024-01-15T00:00:00Z'))

    // 最終処理日を更新
    const updated = await testDb.prisma.autoJournalManagement.update({
      where: { id: management.id },
      data: { lastProcessedAt: new Date('2024-01-16T00:00:00Z') }
    })

    expect(updated.lastProcessedAt).toEqual(new Date('2024-01-16T00:00:00Z'))
  })

  test('日付管理方式: 複数のソーステーブルを管理できる', async () => {
    if (!testDb.prisma) {
      throw new Error('Prisma client not initialized')
    }

    await testDb.prisma.autoJournalManagement.createMany({
      data: [
        {
          sourceTable: '売上データ',
          lastProcessedAt: new Date('2024-01-15T00:00:00Z')
        },
        {
          sourceTable: '給与データ',
          lastProcessedAt: new Date('2024-01-10T00:00:00Z')
        }
      ]
    })

    const records = await testDb.prisma.autoJournalManagement.findMany({
      orderBy: { sourceTable: 'asc' }
    })

    expect(records).toHaveLength(2)
    expect(records[0].sourceTable).toBe('売上データ')
    expect(records[1].sourceTable).toBe('給与データ')
  })

  test('自動仕訳パターンを登録できる', async () => {
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

    const pattern = await testDb.prisma.autoJournalPattern.create({
      data: {
        patternCode: 'PAT001',
        patternName: '売上自動仕訳',
        sourceTable: '売上データ',
        description: '売上データから自動的に仕訳を生成',
        isActive: true,
        items: {
          create: [
            {
              lineNo: 1,
              debitCredit: 'D',
              accountCode: '1100',
              amountExpression: 'amount'
            },
            {
              lineNo: 2,
              debitCredit: 'C',
              accountCode: '4100',
              amountExpression: 'amount'
            }
          ]
        }
      },
      include: { items: true }
    })

    expect(pattern.patternCode).toBe('PAT001')
    expect(pattern.patternName).toBe('売上自動仕訳')
    expect(pattern.items).toHaveLength(2)
    expect(pattern.items[0].accountCode).toBe('1100')
    expect(pattern.items[1].accountCode).toBe('4100')
  })

  test('自動仕訳パターンを無効化できる', async () => {
    if (!testDb.prisma) {
      throw new Error('Prisma client not initialized')
    }

    const pattern = await testDb.prisma.autoJournalPattern.create({
      data: {
        patternCode: 'PAT002',
        patternName: 'テストパターン',
        sourceTable: 'テストデータ',
        isActive: true
      }
    })

    const deactivated = await testDb.prisma.autoJournalPattern.update({
      where: { id: pattern.id },
      data: { isActive: false }
    })

    expect(deactivated.isActive).toBe(false)
  })

  test('有効な自動仕訳パターンのみを取得できる', async () => {
    if (!testDb.prisma) {
      throw new Error('Prisma client not initialized')
    }

    await testDb.prisma.autoJournalPattern.createMany({
      data: [
        {
          patternCode: 'PAT001',
          patternName: '有効パターン1',
          sourceTable: 'データ1',
          isActive: true
        },
        {
          patternCode: 'PAT002',
          patternName: '無効パターン',
          sourceTable: 'データ2',
          isActive: false
        },
        {
          patternCode: 'PAT003',
          patternName: '有効パターン2',
          sourceTable: 'データ3',
          isActive: true
        }
      ]
    })

    const activePatterns = await testDb.prisma.autoJournalPattern.findMany({
      where: { isActive: true }
    })

    expect(activePatterns).toHaveLength(2)
    expect(activePatterns.every((p) => p.isActive)).toBe(true)
  })

  test('自動仕訳ログを記録できる', async () => {
    if (!testDb.prisma) {
      throw new Error('Prisma client not initialized')
    }

    const pattern = await testDb.prisma.autoJournalPattern.create({
      data: {
        patternCode: 'PAT001',
        patternName: 'テストパターン',
        sourceTable: 'テストデータ',
        isActive: true
      }
    })

    const log = await testDb.prisma.autoJournalLog.create({
      data: {
        patternId: pattern.id,
        executedAt: new Date('2024-01-16T12:00:00Z'),
        recordsProcessed: 100,
        journalsCreated: 100,
        status: '成功',
        message: '100件の仕訳を生成しました'
      }
    })

    expect(log.recordsProcessed).toBe(100)
    expect(log.journalsCreated).toBe(100)
    expect(log.status).toBe('成功')
  })

  test('エラーログを記録できる', async () => {
    if (!testDb.prisma) {
      throw new Error('Prisma client not initialized')
    }

    const pattern = await testDb.prisma.autoJournalPattern.create({
      data: {
        patternCode: 'PAT001',
        patternName: 'テストパターン',
        sourceTable: 'テストデータ',
        isActive: true
      }
    })

    const errorLog = await testDb.prisma.autoJournalLog.create({
      data: {
        patternId: pattern.id,
        executedAt: new Date('2024-01-16T12:00:00Z'),
        recordsProcessed: 50,
        journalsCreated: 0,
        status: 'エラー',
        message: 'データベース接続エラー',
        errorDetail: 'Connection timeout'
      }
    })

    expect(errorLog.status).toBe('エラー')
    expect(errorLog.errorDetail).toBe('Connection timeout')
  })
})
