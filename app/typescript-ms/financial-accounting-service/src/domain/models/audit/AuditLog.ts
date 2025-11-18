// src/domain/audit/AuditLog.ts
import { AuditAction } from './AuditAction'

/**
 * 監査ログエンティティ
 *
 * 財務データの全ての変更を追跡するための不変オブジェクト
 */
export class AuditLog {
  private readonly _id?: number
  private readonly _entityType: string
  private readonly _entityId: string
  private readonly _action: AuditAction
  private readonly _userId: string
  private readonly _userName: string
  private readonly _timestamp: Date
  private readonly _oldValues?: Record<string, unknown>
  private readonly _newValues?: Record<string, unknown>
  private readonly _changes?: Record<string, unknown>
  private readonly _reason?: string
  private readonly _ipAddress?: string
  private readonly _userAgent?: string

  private constructor(props: {
    id?: number
    entityType: string
    entityId: string
    action: AuditAction
    userId: string
    userName: string
    timestamp: Date
    oldValues?: Record<string, unknown>
    newValues?: Record<string, unknown>
    changes?: Record<string, unknown>
    reason?: string
    ipAddress?: string
    userAgent?: string
  }) {
    this._id = props.id
    this._entityType = props.entityType
    this._entityId = props.entityId
    this._action = props.action
    this._userId = props.userId
    this._userName = props.userName
    this._timestamp = props.timestamp
    this._oldValues = props.oldValues ? Object.freeze({ ...props.oldValues }) : undefined
    this._newValues = props.newValues ? Object.freeze({ ...props.newValues }) : undefined
    this._changes = props.changes ? Object.freeze({ ...props.changes }) : undefined
    this._reason = props.reason
    this._ipAddress = props.ipAddress
    this._userAgent = props.userAgent

    // オブジェクト全体をフリーズして不変にする
    Object.freeze(this)
  }

  /**
   * 新しい監査ログを作成
   */
  static create(props: {
    entityType: string
    entityId: string
    action: AuditAction
    userId: string
    userName: string
    oldValues?: Record<string, unknown>
    newValues?: Record<string, unknown>
    changes?: Record<string, unknown>
    reason?: string
    ipAddress?: string
    userAgent?: string
  }): AuditLog {
    this.validateRequiredFields(props)
    this.validateDeletionReason(props.action, props.reason)

    return new AuditLog({
      ...props,
      timestamp: new Date()
    })
  }

  /**
   * 必須フィールドのバリデーション
   */
  private static validateRequiredFields(props: {
    entityType: string
    entityId: string
    userId: string
    userName: string
  }): void {
    this.validateEntityFields(props.entityType, props.entityId)
    this.validateUserFields(props.userId, props.userName)
  }

  /**
   * エンティティフィールドのバリデーション
   */
  private static validateEntityFields(entityType: string, entityId: string): void {
    if (!entityType || entityType.trim() === '') {
      throw new Error('エンティティタイプは必須です')
    }
    if (!entityId || entityId.trim() === '') {
      throw new Error('エンティティIDは必須です')
    }
  }

  /**
   * ユーザーフィールドのバリデーション
   */
  private static validateUserFields(userId: string, userName: string): void {
    if (!userId || userId.trim() === '') {
      throw new Error('ユーザーIDは必須です')
    }
    if (!userName || userName.trim() === '') {
      throw new Error('ユーザー名は必須です')
    }
  }

  /**
   * 削除操作の理由バリデーション
   */
  private static validateDeletionReason(action: AuditAction, reason?: string): void {
    if (action === AuditAction.DELETE && !reason) {
      console.warn('削除操作には理由を記録することを推奨します')
    }
  }

  /**
   * データベースから再構成
   */
  static reconstruct(props: {
    id: number
    entityType: string
    entityId: string
    action: AuditAction
    userId: string
    userName: string
    timestamp: Date
    oldValues?: Record<string, unknown>
    newValues?: Record<string, unknown>
    changes?: Record<string, unknown>
    reason?: string
    ipAddress?: string
    userAgent?: string
  }): AuditLog {
    return new AuditLog(props)
  }

  // ゲッター（読み取り専用）
  get id(): number | undefined {
    return this._id
  }

  get entityType(): string {
    return this._entityType
  }

  get entityId(): string {
    return this._entityId
  }

  get action(): AuditAction {
    return this._action
  }

  get userId(): string {
    return this._userId
  }

  get userName(): string {
    return this._userName
  }

  get timestamp(): Date {
    return new Date(this._timestamp)
  }

  get oldValues(): Record<string, unknown> | undefined {
    return this._oldValues ? { ...this._oldValues } : undefined
  }

  get newValues(): Record<string, unknown> | undefined {
    return this._newValues ? { ...this._newValues } : undefined
  }

  get changes(): Record<string, unknown> | undefined {
    return this._changes ? { ...this._changes } : undefined
  }

  get reason(): string | undefined {
    return this._reason
  }

  get ipAddress(): string | undefined {
    return this._ipAddress
  }

  get userAgent(): string | undefined {
    return this._userAgent
  }

  /**
   * 変更内容のサマリーを取得
   */
  getChangeSummary(): string {
    switch (this._action) {
      case AuditAction.CREATE:
        return `${this._entityType} ${this._entityId} を作成`
      case AuditAction.UPDATE:
        return `${this._entityType} ${this._entityId} を更新`
      case AuditAction.DELETE:
        return `${this._entityType} ${this._entityId} を削除`
      case AuditAction.READ:
        return `${this._entityType} ${this._entityId} を参照`
      default:
        return `${this._entityType} ${this._entityId} に対する操作`
    }
  }
}
