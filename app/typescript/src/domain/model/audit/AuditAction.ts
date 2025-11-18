// src/audit/domain/AuditAction.ts

/**
 * 監査ログのアクション種別
 */
export enum AuditAction {
  CREATE = 'CREATE', // 作成
  UPDATE = 'UPDATE', // 更新
  DELETE = 'DELETE', // 削除
  READ = 'READ' // 参照（必要に応じて）
}
