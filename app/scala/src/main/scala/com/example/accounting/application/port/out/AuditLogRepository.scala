package com.example.accounting.application.port.out

import com.example.accounting.domain.audit.AuditLog
import scalikejdbc.DBSession

import java.time.Instant

/**
 * 監査ログリポジトリ（Output Port）
 */
trait AuditLogRepository:

  /** 監査ログを保存 */
  def save(auditLog: AuditLog)(implicit session: DBSession): AuditLog

  /** エンティティで検索 */
  def findByEntity(entityType: String, entityId: String)(implicit session: DBSession): List[AuditLog]

  /** ユーザーと期間で検索 */
  def findByUser(userId: String, startDate: Instant, endDate: Instant)(implicit session: DBSession): List[AuditLog]

  /** 期間で検索 */
  def findByPeriod(startDate: Instant, endDate: Instant, limit: Int)(implicit session: DBSession): List[AuditLog]

  /** アクションで検索 */
  def findByAction(action: String, startDate: Instant, endDate: Instant)(implicit session: DBSession): List[AuditLog]
