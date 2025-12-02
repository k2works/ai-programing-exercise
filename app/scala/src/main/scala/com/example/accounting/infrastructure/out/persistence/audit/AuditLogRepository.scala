package com.example.accounting.infrastructure.out.persistence.audit

import com.example.accounting.application.port.out.{AuditLogRepository => AuditLogRepositoryPort}
import com.example.accounting.domain.audit.{AuditLog, AuditAction}
import scalikejdbc.*

import java.time.Instant

/**
 * 監査ログリポジトリ（ScalikeJDBC 実装）
 */
class AuditLogRepository extends AuditLogRepositoryPort:

  /**
   * 監査ログを保存
   */
  def save(auditLog: AuditLog)(implicit session: DBSession): AuditLog =
    val id = sql"""
      INSERT INTO "監査ログ" (
        "エンティティ種別", "エンティティID", "アクション",
        "ユーザーID", "ユーザー名", "タイムスタンプ",
        "変更前の値", "変更後の値", "変更内容",
        "理由", "IPアドレス", "ユーザーエージェント"
      ) VALUES (
        ${auditLog.entityType},
        ${auditLog.entityId},
        ${auditLog.action.code},
        ${auditLog.userId},
        ${auditLog.userName},
        ${auditLog.timestamp},
        ${auditLog.oldValues}::jsonb,
        ${auditLog.newValues}::jsonb,
        ${auditLog.changes}::jsonb,
        ${auditLog.reason},
        ${auditLog.ipAddress},
        ${auditLog.userAgent}
      )
    """.updateAndReturnGeneratedKey.apply()

    auditLog.copy(id = Some(id))

  /**
   * エンティティで検索
   */
  def findByEntity(entityType: String, entityId: String)(implicit session: DBSession): List[AuditLog] =
    sql"""
      SELECT * FROM "監査ログ"
      WHERE "エンティティ種別" = ${entityType}
        AND "エンティティID" = ${entityId}
      ORDER BY "タイムスタンプ" DESC
    """.map(mapToAuditLog).list.apply()

  /**
   * ユーザーと期間で検索
   */
  def findByUser(userId: String, startDate: Instant, endDate: Instant)(implicit session: DBSession): List[AuditLog] =
    sql"""
      SELECT * FROM "監査ログ"
      WHERE "ユーザーID" = ${userId}
        AND "タイムスタンプ" BETWEEN ${startDate} AND ${endDate}
      ORDER BY "タイムスタンプ" DESC
    """.map(mapToAuditLog).list.apply()

  /**
   * 期間で検索
   */
  def findByPeriod(startDate: Instant, endDate: Instant, limit: Int)(implicit session: DBSession): List[AuditLog] =
    sql"""
      SELECT * FROM "監査ログ"
      WHERE "タイムスタンプ" BETWEEN ${startDate} AND ${endDate}
      ORDER BY "タイムスタンプ" DESC
      LIMIT ${limit}
    """.map(mapToAuditLog).list.apply()

  /**
   * アクションで検索
   */
  def findByAction(action: String, startDate: Instant, endDate: Instant)(implicit session: DBSession): List[AuditLog] =
    sql"""
      SELECT * FROM "監査ログ"
      WHERE "アクション" = ${action}
        AND "タイムスタンプ" BETWEEN ${startDate} AND ${endDate}
      ORDER BY "タイムスタンプ" DESC
    """.map(mapToAuditLog).list.apply()

  private def mapToAuditLog(rs: WrappedResultSet): AuditLog =
    AuditLog(
      id = Some(rs.long("ID")),
      entityType = rs.string("エンティティ種別"),
      entityId = rs.string("エンティティID"),
      action = AuditAction.fromCode(rs.string("アクション")),
      userId = rs.string("ユーザーID"),
      userName = rs.string("ユーザー名"),
      timestamp = rs.offsetDateTime("タイムスタンプ").toInstant,
      oldValues = rs.stringOpt("変更前の値"),
      newValues = rs.stringOpt("変更後の値"),
      changes = rs.stringOpt("変更内容"),
      reason = rs.stringOpt("理由"),
      ipAddress = rs.stringOpt("IPアドレス"),
      userAgent = rs.stringOpt("ユーザーエージェント"),
    )
