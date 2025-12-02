package com.example.accounting.domain.audit

import java.time.Instant

/** 監査ログ（不変） */
case class AuditLog(
    id: Option[Long],
    entityType: String,
    entityId: String,
    action: AuditAction,
    userId: String,
    userName: String,
    timestamp: Instant,
    oldValues: Option[String],
    newValues: Option[String],
    changes: Option[String],
    reason: Option[String],
    ipAddress: Option[String],
    userAgent: Option[String],
)

object AuditLog:
  /** CREATE 操作用のファクトリメソッド */
  def create(
      entityType: String,
      entityId: String,
      userId: String,
      userName: String,
      changes: String,
      ipAddress: Option[String] = None,
      userAgent: Option[String] = None,
  ): AuditLog =
    AuditLog(
      id = None,
      entityType = entityType,
      entityId = entityId,
      action = AuditAction.Create,
      userId = userId,
      userName = userName,
      timestamp = Instant.now(),
      oldValues = None,
      newValues = None,
      changes = Some(changes),
      reason = None,
      ipAddress = ipAddress,
      userAgent = userAgent,
    )

  /** UPDATE 操作用のファクトリメソッド */
  def createForUpdate(
      entityType: String,
      entityId: String,
      userId: String,
      userName: String,
      oldValues: String,
      newValues: String,
      ipAddress: Option[String] = None,
      userAgent: Option[String] = None,
  ): AuditLog =
    AuditLog(
      id = None,
      entityType = entityType,
      entityId = entityId,
      action = AuditAction.Update,
      userId = userId,
      userName = userName,
      timestamp = Instant.now(),
      oldValues = Some(oldValues),
      newValues = Some(newValues),
      changes = None,
      reason = None,
      ipAddress = ipAddress,
      userAgent = userAgent,
    )

  /** DELETE 操作用のファクトリメソッド */
  def createForDelete(
      entityType: String,
      entityId: String,
      userId: String,
      userName: String,
      oldValues: String,
      reason: Option[String] = None,
      ipAddress: Option[String] = None,
      userAgent: Option[String] = None,
  ): AuditLog =
    AuditLog(
      id = None,
      entityType = entityType,
      entityId = entityId,
      action = AuditAction.Delete,
      userId = userId,
      userName = userName,
      timestamp = Instant.now(),
      oldValues = Some(oldValues),
      newValues = None,
      changes = None,
      reason = reason,
      ipAddress = ipAddress,
      userAgent = userAgent,
    )
