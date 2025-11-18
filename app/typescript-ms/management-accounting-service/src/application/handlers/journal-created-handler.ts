// src/application/handlers/journal-created-handler.ts

import { JournalCacheRepository } from '../../domain/models/journal-cache'
import { FinancialAccountingEvent } from '../../domain/models/external/FinancialAccountingEvent'
import { FinancialAccountingEventTranslator } from '../translators/FinancialAccountingEventTranslator'

/**
 * JournalCreated イベントハンドラー
 *
 * 財務会計サービスから仕訳作成イベントを受け取り、
 * 腐敗防止層（ACL）を通してドメインモデルに変換し、
 * 管理会計サービスのデータベースにキャッシュする
 *
 * DDD戦略的設計における境界付けられたコンテキスト間の統合パターン
 */
export class JournalCreatedHandler {
  private readonly translator: FinancialAccountingEventTranslator

  constructor(private journalCacheRepository: JournalCacheRepository) {
    this.translator = new FinancialAccountingEventTranslator()
  }

  /**
   * イベントを処理する
   *
   * 腐敗防止層を通して外部イベントを内部ドメインモデルに変換
   */
  async handle(event: FinancialAccountingEvent): Promise<void> {
    try {
      console.log('🔄 Processing Financial Accounting event:', event)

      // バリデーション
      const validation = this.translator.validateEvent(event)
      if (!validation.valid) {
        console.error('❌ Invalid event:', validation.errors)
        throw new Error(`Event validation failed: ${validation.errors.join(', ')}`)
      }

      // 腐敗防止層: 外部イベント → 内部ドメインモデルに変換
      const journalCache = this.translator.translateToJournalCache(event)

      if (!journalCache) {
        console.warn(`⚠️  Unsupported event type: ${event.eventType}`)
        return
      }

      // ドメインモデルをリポジトリに保存
      await this.journalCacheRepository.save(journalCache)

      console.log(
        `✅ Journal cache saved: journalId=${journalCache.journalId}, fiscalYear=${journalCache.fiscalYear}`
      )
    } catch (error) {
      console.error('❌ Failed to process Financial Accounting event:', error)
      throw error
    }
  }
}
