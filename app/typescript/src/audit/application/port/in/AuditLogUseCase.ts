// src/audit/application/port/in/AuditLogUseCase.ts
import { AuditLog } from '../../../domain/AuditLog'
import { AuditAction } from '../../../domain/AuditAction'

/**
 * 監査ログユースケースインターフェース（Input Port）
 *
 * ヘキサゴナルアーキテクチャの入力ポート
 * アプリケーション層がドメイン層に依存しないようにする
 */
export interface AuditLogUseCase {
  /**
   * エンティティの監査ログを取得
   *
   * @param entityType - エンティティタイプ（Journal, Account等）
   * @param entityId - エンティティID
   * @returns 監査ログの配列（新しい順）
   */
  getAuditLogsByEntity(entityType: string, entityId: string): Promise<AuditLog[]>

  /**
   * ユーザーの操作履歴を取得
   *
   * @param userId - ユーザーID
   * @param startDate - 開始日時（省略可）
   * @param endDate - 終了日時（省略可）
   * @returns 監査ログの配列（新しい順）
   */
  getAuditLogsByUser(userId: string, startDate?: Date, endDate?: Date): Promise<AuditLog[]>

  /**
   * 期間指定で監査ログを取得
   *
   * @param startDate - 開始日時
   * @param endDate - 終了日時
   * @returns 監査ログの配列（新しい順）
   */
  getAuditLogsByPeriod(startDate: Date, endDate: Date): Promise<AuditLog[]>

  /**
   * アクション種別で監査ログを取得
   *
   * @param action - アクション種別
   * @param limit - 取得件数上限（デフォルト: 100）
   * @returns 監査ログの配列（新しい順）
   */
  getAuditLogsByAction(action: AuditAction, limit?: number): Promise<AuditLog[]>
}
