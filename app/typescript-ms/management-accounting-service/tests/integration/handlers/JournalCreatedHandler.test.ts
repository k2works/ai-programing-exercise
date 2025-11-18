// tests/integration/handlers/JournalCreatedHandler.test.ts

import { describe, it, expect, beforeAll, afterAll, beforeEach } from 'vitest'
import { TestContainersSetup } from '../../setup/test-containers'
import { JournalCreatedHandler } from '../../../src/application/handlers/journal-created-handler'
import { PrismaJournalCacheRepository } from '../../../src/infrastructure/persistence/PrismaJournalCacheRepository'
import { JournalCreatedEventDto } from '../../../src/domain/models/external/FinancialAccountingEvent'
import { getPrismaClient, closePrismaClient } from '../../../src/infrastructure/persistence/prisma-client'

describe('JournalCreatedHandler Integration Test', () => {
  let testContainers: TestContainersSetup
  let handler: JournalCreatedHandler
  let repository: PrismaJournalCacheRepository

  beforeAll(async () => {
    // TestContainers を起動
    testContainers = new TestContainersSetup()
    await testContainers.start()

    // ハンドラーとリポジトリを初期化
    repository = new PrismaJournalCacheRepository()
    handler = new JournalCreatedHandler(repository)
  }, 60000)

  afterAll(async () => {
    await closePrismaClient()
    await testContainers.stop()
  })

  beforeEach(async () => {
    // 各テスト前にデータベースをクリーンアップ
    const prisma = getPrismaClient()
    await prisma.journalCache.deleteMany()
  })

  describe('handle', () => {
    it('should process JournalCreatedEvent and save to database', async () => {
      // Arrange
      const event: JournalCreatedEventDto = {
        eventType: 'JournalCreated',
        occurredAt: new Date('2024-01-15T10:00:00Z'),
        payload: {
          journalId: '1',
          fiscalYear: 2024,
          journalDate: '2024-01-15',
          totalDebitAmount: 10000,
          totalCreditAmount: 10000
        }
      }

      // Act
      await handler.handle(event)

      // Assert
      const caches = await repository.findAll()
      expect(caches.length).toBe(1)
      expect(caches[0].journalId).toBe('1')
      expect(caches[0].fiscalYear).toBe(2024)
      expect(caches[0].totalDebitAmount).toBe(10000)
      expect(caches[0].totalCreditAmount).toBe(10000)
    })

    it('should handle journalDate as Date object', async () => {
      // Arrange
      const event: JournalCreatedEventDto = {
        eventType: 'JournalCreated',
        occurredAt: new Date('2024-02-20T10:00:00Z'),
        payload: {
          journalId: '2',
          fiscalYear: 2024,
          journalDate: new Date('2024-02-20'),
          totalDebitAmount: 20000,
          totalCreditAmount: 20000
        }
      }

      // Act
      await handler.handle(event)

      // Assert
      const caches = await repository.findByFiscalYear(2024)
      expect(caches.length).toBe(1)
      expect(caches[0].journalDate).toEqual(new Date('2024-02-20'))
    })

    it('should throw error when event validation fails', async () => {
      // Arrange
      const invalidEvent: JournalCreatedEventDto = {
        eventType: 'JournalCreated',
        occurredAt: new Date(),
        payload: {
          journalId: '',
          fiscalYear: 0,
          journalDate: new Date(),
          totalDebitAmount: -100,
          totalCreditAmount: -100
        }
      }

      // Act & Assert
      await expect(handler.handle(invalidEvent)).rejects.toThrow('Event validation failed')
    })

    it('should throw error when debit and credit amounts do not match', async () => {
      // Arrange
      const event: JournalCreatedEventDto = {
        eventType: 'JournalCreated',
        occurredAt: new Date(),
        payload: {
          journalId: '3',
          fiscalYear: 2024,
          journalDate: new Date(),
          totalDebitAmount: 10000,
          totalCreditAmount: 15000 // 不一致
        }
      }

      // Act & Assert
      await expect(handler.handle(event)).rejects.toThrow('Debit and credit amounts must match')
    })

    it('should ignore unsupported event types', async () => {
      // Arrange
      const unsupportedEvent = {
        eventType: 'AccountCreated',
        occurredAt: new Date(),
        payload: {
          accountCode: '1010',
          accountName: '現金'
        }
      }

      // Act
      await handler.handle(unsupportedEvent as any)

      // Assert
      const caches = await repository.findAll()
      expect(caches.length).toBe(0) // 何も保存されないはず
    })
  })
})
