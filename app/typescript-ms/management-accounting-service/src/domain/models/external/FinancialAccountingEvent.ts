// src/domain/models/external/FinancialAccountingEvent.ts

/**
 * 財務会計サービスから受信するイベントのDTO
 *
 * このDTOは外部システム（財務会計サービス）の形式を表現し、
 * 管理会計サービスのドメインモデルとは独立している
 */

/**
 * 財務会計サービスのドメインイベント基底型
 */
export interface FinancialAccountingEvent {
  eventType: string
  occurredAt: Date | string
}

/**
 * 仕訳作成イベント（財務会計サービス形式）
 */
export interface JournalCreatedEventDto extends FinancialAccountingEvent {
  eventType: 'JournalCreated'
  payload: {
    journalId: string
    fiscalYear: number
    journalDate: Date | string
    totalDebitAmount: number
    totalCreditAmount: number
  }
}

/**
 * 勘定科目作成イベント（財務会計サービス形式）
 */
export interface AccountCreatedEventDto extends FinancialAccountingEvent {
  eventType: 'AccountCreated'
  payload: {
    accountCode: string
    accountName: string
    accountType: string
  }
}

/**
 * 財務会計サービスのイベント型判定
 */
export function isJournalCreatedEvent(
  event: FinancialAccountingEvent
): event is JournalCreatedEventDto {
  return event.eventType === 'JournalCreated'
}

export function isAccountCreatedEvent(
  event: FinancialAccountingEvent
): event is AccountCreatedEventDto {
  return event.eventType === 'AccountCreated'
}
