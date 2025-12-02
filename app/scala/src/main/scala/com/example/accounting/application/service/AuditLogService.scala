package com.example.accounting.application.service

import com.example.accounting.application.*
import com.example.accounting.application.port.in.AuditLogUseCase
import com.example.accounting.application.port.out.AuditLogRepository
import com.example.accounting.domain.audit.AuditLog
import scalikejdbc.*

import java.time.Instant

/**
 * 監査ログサービス（AuditLogUseCase の実装）
 */
class AuditLogService(
    auditLogRepository: AuditLogRepository,
) extends AuditLogUseCase:

  override def recordAuditLog(auditLog: AuditLog): Either[AppError, AuditLog] =
    try
      DB.localTx { implicit session =>
        val saved = auditLogRepository.save(auditLog)
        Right(saved)
      }
    catch
      case e: Exception =>
        Left(DatabaseError("監査ログの記録に失敗しました", Some(e)))

  override def getAuditLogsByEntity(entityType: String, entityId: String): Either[AppError, List[AuditLog]] =
    try
      DB.readOnly { implicit session =>
        val logs = auditLogRepository.findByEntity(entityType, entityId)
        Right(logs)
      }
    catch
      case e: Exception =>
        Left(DatabaseError("監査ログの取得に失敗しました", Some(e)))

  override def getAuditLogsByUser(
      userId: String,
      startDate: Instant,
      endDate: Instant,
  ): Either[AppError, List[AuditLog]] =
    try
      DB.readOnly { implicit session =>
        val logs = auditLogRepository.findByUser(userId, startDate, endDate)
        Right(logs)
      }
    catch
      case e: Exception =>
        Left(DatabaseError("監査ログの取得に失敗しました", Some(e)))

  override def getAuditLogsByPeriod(
      startDate: Instant,
      endDate: Instant,
      limit: Int,
  ): Either[AppError, List[AuditLog]] =
    try
      DB.readOnly { implicit session =>
        val logs = auditLogRepository.findByPeriod(startDate, endDate, limit)
        Right(logs)
      }
    catch
      case e: Exception =>
        Left(DatabaseError("監査ログの取得に失敗しました", Some(e)))

  override def getAuditLogsByAction(
      action: String,
      startDate: Instant,
      endDate: Instant,
  ): Either[AppError, List[AuditLog]] =
    try
      DB.readOnly { implicit session =>
        val logs = auditLogRepository.findByAction(action, startDate, endDate)
        Right(logs)
      }
    catch
      case e: Exception =>
        Left(DatabaseError("監査ログの取得に失敗しました", Some(e)))
