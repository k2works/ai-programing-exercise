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
  private readonly _oldValues?: Record<string, any>
  private readonly _newValues?: Record<string, any>
  private readonly _changes?: Record<string, any>
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
    oldValues?: Record<string, any>
    newValues?: Record<string, any>
    changes?: Record<string, any>
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
    oldValues?: Record<string, any>
    newValues?: Record<string, any>
    changes?: Record<string, any>
    reason?: string
    ipAddress?: string
    userAgent?: string
  }): AuditLog {
    // バリデーション
    if (!props.entityType || props.entityType.trim() === '') {
      throw new Error('エンティティタイプは必須です')
    }
    if (!props.entityId || props.entityId.trim() === '') {
      throw new Error('エンティティIDは必須です')
    }
    if (!props.userId || props.userId.trim() === '') {
      throw new Error('ユーザーIDは必須です')
    }
    if (!props.userName || props.userName.trim() === '') {
      throw new Error('ユーザー名は必須です')
    }

    // 削除操作の場合、理由が推奨される
    if (props.action === AuditAction.DELETE && !props.reason) {
      console.warn('削除操作には理由を記録することを推奨します')
    }

    return new AuditLog({
      ...props,
      timestamp: new Date()
    })
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
    oldValues?: Record<string, any>
    newValues?: Record<string, any>
    changes?: Record<string, any>
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

  get oldValues(): Record<string, any> | undefined {
    return this._oldValues ? { ...this._oldValues } : undefined
  }

  get newValues(): Record<string, any> | undefined {
    return this._newValues ? { ...this._newValues } : undefined
  }

  get changes(): Record<string, any> | undefined {
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
