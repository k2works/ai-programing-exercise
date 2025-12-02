package com.example.accounting.application.port.in

import com.example.accounting.application.AppError
import com.example.accounting.domain.audit.AuditLog

import java.time.Instant

/**
 * 監査ログユースケース（Input Port）
 */
trait AuditLogUseCase:

  /** 監査ログを記録 */
  def recordAuditLog(auditLog: AuditLog): Either[AppError, AuditLog]

  /** エンティティの監査ログを取得 */
  def getAuditLogsByEntity(entityType: String, entityId: String): Either[AppError, List[AuditLog]]

  /** ユーザーの監査ログを取得 */
  def getAuditLogsByUser(userId: String, startDate: Instant, endDate: Instant): Either[AppError, List[AuditLog]]

  /** 期間の監査ログを取得 */
  def getAuditLogsByPeriod(startDate: Instant, endDate: Instant, limit: Int): Either[AppError, List[AuditLog]]

  /** アクションで監査ログを取得 */
  def getAuditLogsByAction(action: String, startDate: Instant, endDate: Instant): Either[AppError, List[AuditLog]]
