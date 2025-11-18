// tests/integration/repository/JournalCacheRepository.test.ts

import { describe, it, expect, beforeAll, afterAll, beforeEach } from 'vitest'
import { TestContainersSetup } from '../../setup/test-containers'
import { PrismaJournalCacheRepository } from '../../../src/infrastructure/persistence/PrismaJournalCacheRepository'
import { JournalCache } from '../../../src/domain/models/journal-cache'
import { getPrismaClient, closePrismaClient } from '../../../src/infrastructure/persistence/prisma-client'

describe('PrismaJournalCacheRepository Integration Test', () => {
  let testContainers: TestContainersSetup
  let repository: PrismaJournalCacheRepository

  beforeAll(async () => {
    // TestContainers を起動
    testContainers = new TestContainersSetup()
    await testContainers.start()

    // リポジトリを初期化
    repository = new PrismaJournalCacheRepository()
  }, 60000) // タイムアウトを60秒に設定（コンテナ起動時間を考慮）

  afterAll(async () => {
    // リソースのクリーンアップ
    await closePrismaClient()
    await testContainers.stop()
  })

  beforeEach(async () => {
    // 各テスト前にデータベースをクリーンアップ
    const prisma = getPrismaClient()
    await prisma.journalCache.deleteMany()
  })

  describe('save', () => {
    it('should save a new journal cache', async () => {
      // Arrange
      const journalCache: JournalCache = {
        journalId: '1',
        fiscalYear: 2024,
        journalDate: new Date('2024-01-15'),
        totalDebitAmount: 10000,
        totalCreditAmount: 10000,
        receivedAt: new Date()
      }

      // Act
      const saved = await repository.save(journalCache)

      // Assert
      expect(saved).toBeDefined()
      expect(saved.journalId).toBe('1')
      expect(saved.fiscalYear).toBe(2024)
      expect(saved.totalDebitAmount).toBe(10000)
      expect(saved.totalCreditAmount).toBe(10000)
    })

    it('should update existing journal cache with same journalId', async () => {
      // Arrange
      const initialCache: JournalCache = {
        journalId: '1',
        fiscalYear: 2024,
        journalDate: new Date('2024-01-15'),
        totalDebitAmount: 10000,
        totalCreditAmount: 10000,
        receivedAt: new Date()
      }
      await repository.save(initialCache)

      const updatedCache: JournalCache = {
        journalId: '1',
        fiscalYear: 2024,
        journalDate: new Date('2024-01-15'),
        totalDebitAmount: 20000,
        totalCreditAmount: 20000,
        receivedAt: new Date()
      }

      // Act
      const saved = await repository.save(updatedCache)

      // Assert
      expect(saved.totalDebitAmount).toBe(20000)
      expect(saved.totalCreditAmount).toBe(20000)

      // データベースに1件のみ存在することを確認
      const all = await repository.findAll()
      expect(all.length).toBe(1)
    })
  })

  describe('findByFiscalYear', () => {
    it('should find journal caches by fiscal year', async () => {
      // Arrange
      await repository.save({
        journalId: '1',
        fiscalYear: 2024,
        journalDate: new Date('2024-01-15'),
        totalDebitAmount: 10000,
        totalCreditAmount: 10000,
        receivedAt: new Date()
      })

      await repository.save({
        journalId: '2',
        fiscalYear: 2024,
        journalDate: new Date('2024-02-20'),
        totalDebitAmount: 20000,
        totalCreditAmount: 20000,
        receivedAt: new Date()
      })

      await repository.save({
        journalId: '3',
        fiscalYear: 2023,
        journalDate: new Date('2023-12-31'),
        totalDebitAmount: 15000,
        totalCreditAmount: 15000,
        receivedAt: new Date()
      })

      // Act
      const caches2024 = await repository.findByFiscalYear(2024)
      const caches2023 = await repository.findByFiscalYear(2023)

      // Assert
      expect(caches2024.length).toBe(2)
      expect(caches2023.length).toBe(1)
      expect(caches2024[0].journalDate).toEqual(new Date('2024-01-15'))
      expect(caches2024[1].journalDate).toEqual(new Date('2024-02-20'))
    })

    it('should return empty array when no caches exist for fiscal year', async () => {
      // Act
      const caches = await repository.findByFiscalYear(2025)

      // Assert
      expect(caches).toEqual([])
    })
  })

  describe('findAll', () => {
    it('should find all journal caches ordered by receivedAt desc', async () => {
      // Arrange
      const cache1 = await repository.save({
        journalId: '1',
        fiscalYear: 2024,
        journalDate: new Date('2024-01-15'),
        totalDebitAmount: 10000,
        totalCreditAmount: 10000,
        receivedAt: new Date('2024-01-15T10:00:00Z')
      })

      await new Promise((resolve) => setTimeout(resolve, 10)) // 確実に時刻が異なるようにする

      const cache2 = await repository.save({
        journalId: '2',
        fiscalYear: 2024,
        journalDate: new Date('2024-02-20'),
        totalDebitAmount: 20000,
        totalCreditAmount: 20000,
        receivedAt: new Date('2024-02-20T10:00:00Z')
      })

      // Act
      const allCaches = await repository.findAll()

      // Assert
      expect(allCaches.length).toBe(2)
      // receivedAt の降順でソートされていることを確認
      expect(allCaches[0].journalId).toBe(cache2.journalId)
      expect(allCaches[1].journalId).toBe(cache1.journalId)
    })
  })
})
