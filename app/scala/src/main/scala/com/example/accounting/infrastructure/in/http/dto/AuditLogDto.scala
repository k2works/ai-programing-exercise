package com.example.accounting.infrastructure.in.http.dto

import com.example.accounting.domain.audit.AuditLog

import java.time.Instant

/**
 * 監査ログレスポンス DTO
 */
case class AuditLogResponse(
    id: Option[Long],
    entityType: String,
    entityId: String,
    action: String,
    userId: String,
    userName: String,
    timestamp: String,
    oldValues: Option[String],
    newValues: Option[String],
    changes: Option[String],
    reason: Option[String],
    ipAddress: Option[String],
)

object AuditLogResponse:
  def fromDomain(auditLog: AuditLog): AuditLogResponse =
    AuditLogResponse(
      id = auditLog.id,
      entityType = auditLog.entityType,
      entityId = auditLog.entityId,
      action = auditLog.action.code,
      userId = auditLog.userId,
      userName = auditLog.userName,
      timestamp = auditLog.timestamp.toString,
      oldValues = auditLog.oldValues,
      newValues = auditLog.newValues,
      changes = auditLog.changes,
      reason = auditLog.reason,
      ipAddress = auditLog.ipAddress,
    )

/**
 * 監査ログ検索リクエスト DTO
 */
case class AuditLogSearchRequest(
    entityType: Option[String],
    entityId: Option[String],
    userId: Option[String],
    action: Option[String],
    startDate: Option[String],
    endDate: Option[String],
    limit: Option[Int],
)
