// src/audit/application/service/AuditLogService.ts
import { AuditLogUseCase } from '../port/in/AuditLogUseCase'
import { AuditLogRepository } from '../../infrastructure/persistence/AuditLogRepository'
import { AuditLog } from '../../domain/AuditLog'
import { AuditAction } from '../../domain/AuditAction'

/**
 * 監査ログアプリケーションサービス
 *
 * Input Port を実装し、監査ログのユースケースを提供する
 * ビジネスルールの検証とリポジトリへの委譲を行う
 */
export class AuditLogService implements AuditLogUseCase {
  constructor(private readonly auditLogRepository: AuditLogRepository) {}

  async getAuditLogsByEntity(entityType: string, entityId: string): Promise<AuditLog[]> {
    // バリデーション
    if (!entityType || !entityId) {
      throw new Error('エンティティタイプとIDは必須です')
    }

    return await this.auditLogRepository.findByEntity(entityType, entityId)
  }

  async getAuditLogsByUser(userId: string, startDate?: Date, endDate?: Date): Promise<AuditLog[]> {
    // バリデーション
    if (!userId) {
      throw new Error('ユーザーIDは必須です')
    }

    // 日付の妥当性チェック
    if (startDate && endDate && startDate > endDate) {
      throw new Error('開始日は終了日より前である必要があります')
    }

    return await this.auditLogRepository.findByUser(userId, startDate, endDate)
  }

  async getAuditLogsByPeriod(startDate: Date, endDate: Date): Promise<AuditLog[]> {
    // バリデーション
    if (!startDate || !endDate) {
      throw new Error('開始日と終了日は必須です')
    }

    if (startDate > endDate) {
      throw new Error('開始日は終了日より前である必要があります')
    }

    // 検索期間の上限チェック（パフォーマンス考慮）
    const daysDiff = Math.floor((endDate.getTime() - startDate.getTime()) / (1000 * 60 * 60 * 24))
    if (daysDiff > 365) {
      throw new Error('検索期間は1年以内に制限されています')
    }

    return await this.auditLogRepository.findByPeriod(startDate, endDate)
  }

  async getAuditLogsByAction(action: AuditAction, limit: number = 100): Promise<AuditLog[]> {
    // バリデーション
    if (limit > 1000) {
      throw new Error('取得件数の上限は1000件です')
    }

    return await this.auditLogRepository.findByAction(action, limit)
  }
}
