// tests/e2e/multi-service.test.ts

import { describe, it, expect, beforeAll, afterAll, beforeEach } from 'vitest'
import { TestContainersSetup } from '../setup/test-containers'
import { PrismaJournalCacheRepository } from '../../src/infrastructure/persistence/PrismaJournalCacheRepository'
import { getPrismaClient, closePrismaClient } from '../../src/infrastructure/persistence/prisma-client'
import { EventSubscriber } from '../../src/infrastructure/messaging/EventSubscriber'
import { JournalCreatedHandler } from '../../src/application/handlers/journal-created-handler'
import amqp, { Connection, Channel } from 'amqplib'

/**
 * マルチサービス統合テスト
 *
 * 財務会計サービスと管理会計サービス間のイベント駆動連携を検証
 * RabbitMQ を介したイベントのパブリッシュ・サブスクライブを実際に実行
 */
describe('Multi-Service Integration Test', () => {
  let testContainers: TestContainersSetup
  let repository: PrismaJournalCacheRepository
  let eventSubscriber: EventSubscriber
  let rabbitmqConnection: Connection
  let publisherChannel: Channel

  beforeAll(async () => {
    console.log('🚀 Starting multi-service integration test setup...')

    // TestContainers を起動
    testContainers = new TestContainersSetup()
    await testContainers.start()

    const connectionInfo = testContainers.getConnectionInfo()

    // 環境変数を設定
    process.env.RABBITMQ_URL = connectionInfo.rabbitmqUrl

    // リポジトリを初期化
    repository = new PrismaJournalCacheRepository()

    // RabbitMQ パブリッシャー接続を確立（財務会計サービスをシミュレート）
    rabbitmqConnection = await amqp.connect(connectionInfo.rabbitmqUrl)
    publisherChannel = await rabbitmqConnection.createChannel()

    // Exchange を作成（財務会計サービスと同じ設定）
    await publisherChannel.assertExchange('financial-accounting-events', 'topic', {
      durable: true
    })

    // EventSubscriber を起動（管理会計サービス）
    const handler = new JournalCreatedHandler(repository)
    eventSubscriber = new EventSubscriber('financial-accounting-events', 'management-accounting-test-queue')
    await eventSubscriber.connect()

    // イベントハンドラーを登録
    eventSubscriber.on('journal.created', (event) => handler.handle(event))

    // イベントの購読を開始
    await eventSubscriber.subscribe()

    // サブスクライバーが準備完了するまで少し待つ
    await new Promise((resolve) => setTimeout(resolve, 1000))

    console.log('✅ Multi-service test environment ready')
  }, 120000) // 2分のタイムアウト

  afterAll(async () => {
    console.log('🛑 Cleaning up multi-service test environment...')

    // EventSubscriber を停止
    if (eventSubscriber) {
      await eventSubscriber.close()
    }

    // RabbitMQ 接続を閉じる
    if (publisherChannel) {
      await publisherChannel.close()
    }
    if (rabbitmqConnection) {
      await rabbitmqConnection.close()
    }

    // Prisma クライアントを閉じる
    await closePrismaClient()

    // TestContainers を停止
    await testContainers.stop()

    console.log('✅ Cleanup complete')
  })

  beforeEach(async () => {
    // 各テスト前にデータベースをクリーンアップ
    const prisma = getPrismaClient()
    await prisma.journalCache.deleteMany()
  })

  describe('Event-Driven Communication', () => {
    it('should propagate JournalCreated event from publisher to subscriber', async () => {
      // Arrange: JournalCreated イベントを準備
      const event = {
        eventType: 'JournalCreated',
        occurredAt: new Date('2024-01-15T10:00:00Z'),
        payload: {
          journalId: '100',
          fiscalYear: 2024,
          journalDate: '2024-01-15',
          totalDebitAmount: 10000,
          totalCreditAmount: 10000
        }
      }

      // Act: イベントをパブリッシュ（財務会計サービスをシミュレート）
      publisherChannel.publish(
        'financial-accounting-events',
        'journal.created',
        Buffer.from(JSON.stringify(event)),
        { persistent: true }
      )

      console.log('📤 Published JournalCreated event:', event)

      // イベント処理を待つ（非同期処理のため）
      await new Promise((resolve) => setTimeout(resolve, 2000))

      // Assert: 管理会計サービスのキャッシュテーブルに仕訳が保存されていることを確認
      const caches = await repository.findByFiscalYear(2024)
      expect(caches.length).toBe(1)
      expect(caches[0].journalId).toBe('100')
      expect(caches[0].fiscalYear).toBe(2024)
      expect(caches[0].totalDebitAmount).toBe(10000)
      expect(caches[0].totalCreditAmount).toBe(10000)
      expect(caches[0].journalDate).toEqual(new Date('2024-01-15'))

      console.log('✅ Event successfully processed and cached')
    }, 30000)

    it('should handle multiple journal events in sequence', async () => {
      // Arrange: 複数のイベントを準備
      const events = [
        {
          eventType: 'JournalCreated',
          occurredAt: new Date('2024-01-15T10:00:00Z'),
          payload: {
            journalId: '101',
            fiscalYear: 2024,
            journalDate: '2024-01-15',
            totalDebitAmount: 10000,
            totalCreditAmount: 10000
          }
        },
        {
          eventType: 'JournalCreated',
          occurredAt: new Date('2024-02-20T10:00:00Z'),
          payload: {
            journalId: '102',
            fiscalYear: 2024,
            journalDate: '2024-02-20',
            totalDebitAmount: 20000,
            totalCreditAmount: 20000
          }
        },
        {
          eventType: 'JournalCreated',
          occurredAt: new Date('2024-03-25T10:00:00Z'),
          payload: {
            journalId: '103',
            fiscalYear: 2024,
            journalDate: '2024-03-25',
            totalDebitAmount: 15000,
            totalCreditAmount: 15000
          }
        }
      ]

      // Act: すべてのイベントをパブリッシュ
      for (const event of events) {
        publisherChannel.publish(
          'financial-accounting-events',
          'journal.created',
          Buffer.from(JSON.stringify(event)),
          { persistent: true }
        )
        console.log('📤 Published event:', event.payload.journalId)
      }

      // イベント処理を待つ
      await new Promise((resolve) => setTimeout(resolve, 3000))

      // Assert: すべての仕訳がキャッシュされていることを確認
      const caches = await repository.findByFiscalYear(2024)
      expect(caches.length).toBe(3)

      // 金額の合計を確認
      const totalDebit = caches.reduce((sum, cache) => sum + cache.totalDebitAmount, 0)
      const totalCredit = caches.reduce((sum, cache) => sum + cache.totalCreditAmount, 0)
      expect(totalDebit).toBe(45000)
      expect(totalCredit).toBe(45000)

      // journalId の確認
      const journalIds = caches.map((c) => c.journalId).sort()
      expect(journalIds).toEqual(['101', '102', '103'])

      console.log('✅ All events successfully processed')
    }, 30000)

    it('should ignore unsupported event types', async () => {
      // Arrange: サポートされていないイベントを準備
      const unsupportedEvent = {
        eventType: 'AccountCreated',
        occurredAt: new Date(),
        payload: {
          accountCode: '1010',
          accountName: '現金'
        }
      }

      // Act: イベントをパブリッシュ
      publisherChannel.publish(
        'financial-accounting-events',
        'account.created',
        Buffer.from(JSON.stringify(unsupportedEvent)),
        { persistent: true }
      )

      console.log('📤 Published unsupported event:', unsupportedEvent)

      // イベント処理を待つ
      await new Promise((resolve) => setTimeout(resolve, 2000))

      // Assert: 何も保存されていないことを確認
      const caches = await repository.findAll()
      expect(caches.length).toBe(0)

      console.log('✅ Unsupported event correctly ignored')
    }, 30000)

    it('should reject invalid events with validation errors', async () => {
      // Arrange: 無効なイベントを準備（貸借不一致）
      const invalidEvent = {
        eventType: 'JournalCreated',
        occurredAt: new Date(),
        payload: {
          journalId: '200',
          fiscalYear: 2024,
          journalDate: '2024-01-15',
          totalDebitAmount: 10000,
          totalCreditAmount: 15000 // 不一致
        }
      }

      // Act: イベントをパブリッシュ
      publisherChannel.publish(
        'financial-accounting-events',
        'journal.created',
        Buffer.from(JSON.stringify(invalidEvent)),
        { persistent: true }
      )

      console.log('📤 Published invalid event:', invalidEvent)

      // イベント処理を待つ
      await new Promise((resolve) => setTimeout(resolve, 2000))

      // Assert: バリデーションエラーで保存されていないことを確認
      const caches = await repository.findAll()
      expect(caches.length).toBe(0)

      console.log('✅ Invalid event correctly rejected')
    }, 30000)

    it('should handle events across different fiscal years', async () => {
      // Arrange: 異なる会計年度のイベントを準備
      const events = [
        {
          eventType: 'JournalCreated',
          occurredAt: new Date('2023-12-31T10:00:00Z'),
          payload: {
            journalId: '301',
            fiscalYear: 2023,
            journalDate: '2023-12-31',
            totalDebitAmount: 5000,
            totalCreditAmount: 5000
          }
        },
        {
          eventType: 'JournalCreated',
          occurredAt: new Date('2024-01-15T10:00:00Z'),
          payload: {
            journalId: '302',
            fiscalYear: 2024,
            journalDate: '2024-01-15',
            totalDebitAmount: 10000,
            totalCreditAmount: 10000
          }
        }
      ]

      // Act: イベントをパブリッシュ
      for (const event of events) {
        publisherChannel.publish(
          'financial-accounting-events',
          'journal.created',
          Buffer.from(JSON.stringify(event)),
          { persistent: true }
        )
      }

      // イベント処理を待つ
      await new Promise((resolve) => setTimeout(resolve, 2000))

      // Assert: 会計年度ごとに確認
      const caches2023 = await repository.findByFiscalYear(2023)
      const caches2024 = await repository.findByFiscalYear(2024)

      expect(caches2023.length).toBe(1)
      expect(caches2023[0].journalId).toBe('301')
      expect(caches2023[0].fiscalYear).toBe(2023)

      expect(caches2024.length).toBe(1)
      expect(caches2024[0].journalId).toBe('302')
      expect(caches2024[0].fiscalYear).toBe(2024)

      console.log('✅ Events correctly separated by fiscal year')
    }, 30000)
  })

  describe('RabbitMQ Infrastructure', () => {
    it('should verify exchange and queue setup', async () => {
      // RabbitMQ の Exchange が正しく設定されていることを確認
      const exchangeCheck = await publisherChannel.checkExchange('financial-accounting-events')
      expect(exchangeCheck).toBeDefined()

      console.log('✅ Exchange configuration verified')
    })

    it('should handle message persistence', async () => {
      // Arrange: 永続化メッセージを準備
      const event = {
        eventType: 'JournalCreated',
        occurredAt: new Date('2024-01-15T10:00:00Z'),
        payload: {
          journalId: '400',
          fiscalYear: 2024,
          journalDate: '2024-01-15',
          totalDebitAmount: 50000,
          totalCreditAmount: 50000
        }
      }

      // Act: 永続化オプションでパブリッシュ
      publisherChannel.publish(
        'financial-accounting-events',
        'journal.created',
        Buffer.from(JSON.stringify(event)),
        {
          persistent: true,
          contentType: 'application/json'
        }
      )

      // イベント処理を待つ
      await new Promise((resolve) => setTimeout(resolve, 2000))

      // Assert: メッセージが処理されたことを確認
      const caches = await repository.findByFiscalYear(2024)
      expect(caches.length).toBe(1)
      expect(caches[0].journalId).toBe('400')

      console.log('✅ Persistent message handling verified')
    }, 30000)
  })
})
