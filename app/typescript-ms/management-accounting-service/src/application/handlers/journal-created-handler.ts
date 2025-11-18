// src/application/handlers/journal-created-handler.ts

import { JournalCacheRepository } from '../../domain/models/journal-cache'

/**
 * JournalCreated イベントのペイロード型定義
 */
export interface JournalCreatedEventPayload {
  journalId: string
  fiscalYear: number
  journalDate: string | Date
  totalDebitAmount: number
  totalCreditAmount: number
}

/**
 * JournalCreated イベントの型定義
 */
export interface JournalCreatedEvent {
  eventType: 'JournalCreated'
  occurredAt: string | Date
  payload: JournalCreatedEventPayload
}

/**
 * JournalCreated イベントハンドラー
 *
 * 財務会計サービスから仕訳作成イベントを受け取り、
 * 管理会計サービスのデータベースにキャッシュする
 */
export class JournalCreatedHandler {
  constructor(private journalCacheRepository: JournalCacheRepository) {}

  /**
   * イベントを処理する
   */
  async handle(event: JournalCreatedEvent): Promise<void> {
    try {
      console.log('🔄 Processing JournalCreated event:', event)

      const { payload } = event

      // イベントデータをキャッシュに保存
      await this.journalCacheRepository.save({
        journalId: payload.journalId,
        fiscalYear: payload.fiscalYear,
        journalDate: new Date(payload.journalDate),
        totalDebitAmount: payload.totalDebitAmount,
        totalCreditAmount: payload.totalCreditAmount,
        receivedAt: new Date()
      })

      console.log(`✅ Journal cache saved: journalId=${payload.journalId}, fiscalYear=${payload.fiscalYear}`)
    } catch (error) {
      console.error('❌ Failed to process JournalCreated event:', error)
      throw error
    }
  }
}
