// src/application/translators/FinancialAccountingEventTranslator.ts

import {
  FinancialAccountingEvent,
  JournalCreatedEventDto,
  isJournalCreatedEvent
} from '../../domain/models/external/FinancialAccountingEvent'
import { JournalCache } from '../../domain/models/journal-cache'

/**
 * 財務会計イベントトランスレーター
 *
 * 腐敗防止層の一部として、外部システム（財務会計サービス）のイベント形式を
 * 管理会計サービスのドメインモデルに変換する責務を持つ
 *
 * DDD戦略的設計における Anti-Corruption Layer (ACL) パターンの実装
 */
export class FinancialAccountingEventTranslator {
  /**
   * JournalCreatedEvent を JournalCache に変換
   *
   * @param event 財務会計サービスのイベント
   * @returns 管理会計サービスのドメインモデル
   * @throws Error イベント形式が不正な場合
   */
  translateToJournalCache(event: FinancialAccountingEvent): JournalCache | null {
    if (!isJournalCreatedEvent(event)) {
      console.warn(`Unsupported event type: ${event.eventType}`)
      return null
    }

    return this.fromJournalCreatedEvent(event)
  }

  /**
   * JournalCreatedEventDto から JournalCache への変換
   */
  private fromJournalCreatedEvent(event: JournalCreatedEventDto): JournalCache {
    const { payload } = event

    // 日付の正規化: string または Date を Date に変換
    const journalDate =
      typeof payload.journalDate === 'string' ? new Date(payload.journalDate) : payload.journalDate

    // 管理会計サービスのドメインモデルに変換
    return {
      journalId: payload.journalId,
      fiscalYear: payload.fiscalYear,
      journalDate,
      totalDebitAmount: payload.totalDebitAmount,
      totalCreditAmount: payload.totalCreditAmount,
      receivedAt: new Date()
    }
  }

  /**
   * バリデーション: イベントデータの妥当性チェック
   */
  validateEvent(event: FinancialAccountingEvent): { valid: boolean; errors: string[] } {
    const errors: string[] = []

    if (!event.eventType) {
      errors.push('eventType is required')
    }

    if (!event.occurredAt) {
      errors.push('occurredAt is required')
    }

    if (isJournalCreatedEvent(event)) {
      const { payload } = event

      if (!payload.journalId) {
        errors.push('payload.journalId is required')
      }

      if (!payload.fiscalYear || payload.fiscalYear <= 0) {
        errors.push('payload.fiscalYear must be a positive number')
      }

      if (!payload.journalDate) {
        errors.push('payload.journalDate is required')
      }

      if (payload.totalDebitAmount < 0) {
        errors.push('payload.totalDebitAmount must be non-negative')
      }

      if (payload.totalCreditAmount < 0) {
        errors.push('payload.totalCreditAmount must be non-negative')
      }

      // 貸借一致チェック（許容誤差: 0.01）
      const difference = Math.abs(payload.totalDebitAmount - payload.totalCreditAmount)
      if (difference > 0.01) {
        errors.push(
          `Debit and credit amounts must match (difference: ${difference.toFixed(2)})`
        )
      }
    }

    return {
      valid: errors.length === 0,
      errors
    }
  }
}
