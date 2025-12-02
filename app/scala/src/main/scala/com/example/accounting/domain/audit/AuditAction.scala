package com.example.accounting.domain.audit

/** 監査アクション種別 */
enum AuditAction(val code: String, val displayName: String):
  case Create extends AuditAction("CREATE", "作成")
  case Update extends AuditAction("UPDATE", "更新")
  case Delete extends AuditAction("DELETE", "削除")

object AuditAction:
  def fromCode(code: String): AuditAction =
    code match
      case "CREATE" => Create
      case "UPDATE" => Update
      case "DELETE" => Delete
      case _        => throw new IllegalArgumentException(s"Unknown audit action: $code")
