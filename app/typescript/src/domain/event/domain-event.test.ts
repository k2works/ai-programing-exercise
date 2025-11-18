// src/domain/event/domain-event.test.ts
import { describe, test, expect } from 'vitest'
import { JournalCreatedEvent, AccountUpdatedEvent, JournalDeletedEvent } from './DomainEvent'

describe('ドメインイベント', () => {
  describe('レッド：DomainEvent の基本機能', () => {
    test('JournalCreatedEvent を作成できる', () => {
      const event = new JournalCreatedEvent(
        1,
        new Date('2025-01-15'),
        '売上計上',
        [
          { accountCode: '1001', debitOrCredit: '借方', amount: 100000 },
          { accountCode: '4001', debitOrCredit: '貸方', amount: 100000 }
        ],
        'user001',
        '山田太郎'
      )

      expect(event.eventType).toBe('JournalCreated')
      expect(event.journalId).toBe(1)
      expect(event.journalDate).toEqual(new Date('2025-01-15'))
      expect(event.description).toBe('売上計上')
      expect(event.userId).toBe('user001')
      expect(event.userName).toBe('山田太郎')
      expect(event.details).toHaveLength(2)
    })

    test('AccountUpdatedEvent を作成できる', () => {
      const oldValues = { accountName: '現金' }
      const newValues = { accountName: '現金及び預金' }

      const event = new AccountUpdatedEvent('1001', oldValues, newValues, 'user001', '山田太郎')

      expect(event.eventType).toBe('AccountUpdated')
      expect(event.accountCode).toBe('1001')
      expect(event.oldValues).toEqual(oldValues)
      expect(event.newValues).toEqual(newValues)
      expect(event.userId).toBe('user001')
      expect(event.userName).toBe('山田太郎')
    })

    test('JournalDeletedEvent を作成できる', () => {
      const journalData = {
        journalDate: '2025-01-15',
        description: '売上計上'
      }

      const event = new JournalDeletedEvent(
        1,
        journalData,
        '入力ミスによる削除',
        'user001',
        '山田太郎'
      )

      expect(event.eventType).toBe('JournalDeleted')
      expect(event.journalId).toBe(1)
      expect(event.journalData).toEqual(journalData)
      expect(event.reason).toBe('入力ミスによる削除')
      expect(event.userId).toBe('user001')
      expect(event.userName).toBe('山田太郎')
    })

    test('イベントには自動的にタイムスタンプが設定される', () => {
      const beforeCreate = new Date()

      const event = new JournalCreatedEvent(
        1,
        new Date('2025-01-15'),
        '売上計上',
        [],
        'user001',
        '山田太郎'
      )

      const afterCreate = new Date()

      expect(event.occurredAt.getTime()).toBeGreaterThanOrEqual(beforeCreate.getTime())
      expect(event.occurredAt.getTime()).toBeLessThanOrEqual(afterCreate.getTime())
    })

    test('イベントには一意のイベント ID が設定される', () => {
      const event1 = new JournalCreatedEvent(
        1,
        new Date('2025-01-15'),
        '売上計上',
        [],
        'user001',
        '山田太郎'
      )

      const event2 = new JournalCreatedEvent(
        2,
        new Date('2025-01-16'),
        '仕入計上',
        [],
        'user002',
        '佐藤花子'
      )

      expect(event1.eventId).toBeDefined()
      expect(event2.eventId).toBeDefined()
      expect(event1.eventId).not.toBe(event2.eventId)
    })

    test('occurredAt は新しい Date オブジェクトを返す（不変性）', () => {
      const event = new JournalCreatedEvent(
        1,
        new Date('2025-01-15'),
        '売上計上',
        [],
        'user001',
        '山田太郎'
      )

      const timestamp1 = event.occurredAt
      const timestamp2 = event.occurredAt

      expect(timestamp1).not.toBe(timestamp2) // 異なるオブジェクト
      expect(timestamp1.getTime()).toBe(timestamp2.getTime()) // 同じ時刻
    })
  })
})
