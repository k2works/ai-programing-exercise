package com.example.accounting.infrastructure.journal

import com.example.accounting.domain.journal.*
import scalikejdbc.*

import java.time.LocalDateTime

/**
 * 自動仕訳リポジトリ
 */
class AutoJournalRepository:

  // ==================== 自動仕訳管理 ====================

  /**
   * 自動仕訳管理を登録
   */
  def insertManagement(management: AutoJournalManagement)(implicit session: DBSession): Int =
    sql"""
      INSERT INTO "自動仕訳管理" (
        "自動仕訳管理ID", "自動仕訳名", "ソーステーブル名",
        "最終処理日時", "有効フラグ"
      ) VALUES (
        ${management.managementId}, ${management.name}, ${management.sourceTable},
        ${management.lastProcessedAt}, ${if (management.enabled) 1 else 0}
      )
    """.update.apply()

  /**
   * 自動仕訳管理を取得
   */
  def findManagementById(id: String)(implicit session: DBSession): Option[AutoJournalManagement] =
    sql"""
      SELECT * FROM "自動仕訳管理" WHERE "自動仕訳管理ID" = ${id}
    """.map(mapToManagement).single.apply()

  /**
   * 有効な自動仕訳管理を取得
   */
  def findEnabledManagements()(implicit session: DBSession): List[AutoJournalManagement] =
    sql"""
      SELECT * FROM "自動仕訳管理" WHERE "有効フラグ" = 1
      ORDER BY "自動仕訳管理ID"
    """.map(mapToManagement).list.apply()

  /**
   * 最終処理日時を更新
   */
  def updateLastProcessedAt(id: String, lastProcessedAt: LocalDateTime)(implicit session: DBSession): Int =
    sql"""
      UPDATE "自動仕訳管理"
      SET "最終処理日時" = ${lastProcessedAt}, "更新日時" = CURRENT_TIMESTAMP
      WHERE "自動仕訳管理ID" = ${id}
    """.update.apply()

  /**
   * 自動仕訳管理を削除
   */
  def deleteManagement(id: String)(implicit session: DBSession): Int =
    sql"""DELETE FROM "自動仕訳管理" WHERE "自動仕訳管理ID" = ${id}""".update.apply()

  // ==================== 自動仕訳パターン ====================

  /**
   * パターンを登録
   */
  def insertPattern(pattern: AutoJournalPattern)(implicit session: DBSession): Int =
    sql"""
      INSERT INTO "自動仕訳パターン" (
        "パターンID", "自動仕訳管理ID", "パターン名",
        "条件式", "優先順位", "有効フラグ"
      ) VALUES (
        ${pattern.patternId}, ${pattern.managementId}, ${pattern.patternName},
        ${pattern.condition}, ${pattern.priority}, ${if (pattern.enabled) 1 else 0}
      )
    """.update.apply()

  /**
   * 管理IDでパターンを取得
   */
  def findPatternsByManagementId(managementId: String)(implicit session: DBSession): List[AutoJournalPattern] =
    sql"""
      SELECT * FROM "自動仕訳パターン"
      WHERE "自動仕訳管理ID" = ${managementId}
      ORDER BY "優先順位", "パターンID"
    """.map(mapToPattern).list.apply()

  /**
   * パターンを取得
   */
  def findPatternById(id: String)(implicit session: DBSession): Option[AutoJournalPattern] =
    sql"""
      SELECT * FROM "自動仕訳パターン" WHERE "パターンID" = ${id}
    """.map(mapToPattern).single.apply()

  // ==================== 自動仕訳パターン明細 ====================

  /**
   * パターン明細を登録
   */
  def insertPatternDetail(detail: AutoJournalPatternDetail)(implicit session: DBSession): Int =
    sql"""
      INSERT INTO "自動仕訳パターン明細" (
        "パターンID", "行番号", "貸借区分",
        "勘定科目コード", "金額式", "摘要テンプレート"
      ) VALUES (
        ${detail.patternId}, ${detail.lineNumber}, ${detail.debitCreditType.code},
        ${detail.accountCode}, ${detail.amountExpression}, ${detail.descriptionTemplate}
      )
    """.update.apply()

  /**
   * パターンIDで明細を取得
   */
  def findPatternDetailsByPatternId(patternId: String)(implicit session: DBSession): List[AutoJournalPatternDetail] =
    sql"""
      SELECT * FROM "自動仕訳パターン明細"
      WHERE "パターンID" = ${patternId}
      ORDER BY "行番号"
    """.map(mapToPatternDetail).list.apply()

  // ==================== 実行ログ ====================

  /**
   * 実行ログを登録
   */
  def insertExecutionLog(log: AutoJournalExecutionLog)(implicit session: DBSession): Long =
    sql"""
      INSERT INTO "自動仕訳実行ログ" (
        "自動仕訳管理ID", "実行開始日時", "実行終了日時",
        "処理件数", "成功件数", "エラー件数", "実行結果", "エラーメッセージ"
      ) VALUES (
        ${log.managementId}, ${log.startTime}, ${log.endTime},
        ${log.totalCount}, ${log.successCount}, ${log.errorCount},
        ${log.result.code}, ${log.errorMessage}
      )
      RETURNING "実行ログID"
    """.map(_.long("実行ログID")).single.apply().getOrElse(0L)

  /**
   * 実行ログを更新（終了時）
   */
  def updateExecutionLog(
      logId: Long,
      endTime: LocalDateTime,
      totalCount: Int,
      successCount: Int,
      errorCount: Int,
      result: ExecutionResult,
      errorMessage: Option[String],
  )(implicit session: DBSession): Int =
    sql"""
      UPDATE "自動仕訳実行ログ"
      SET "実行終了日時" = ${endTime},
          "処理件数" = ${totalCount},
          "成功件数" = ${successCount},
          "エラー件数" = ${errorCount},
          "実行結果" = ${result.code},
          "エラーメッセージ" = ${errorMessage}
      WHERE "実行ログID" = ${logId}
    """.update.apply()

  /**
   * 管理IDで実行ログを取得
   */
  def findLogsByManagementId(managementId: String)(implicit session: DBSession): List[AutoJournalExecutionLog] =
    sql"""
      SELECT * FROM "自動仕訳実行ログ"
      WHERE "自動仕訳管理ID" = ${managementId}
      ORDER BY "実行開始日時" DESC
    """.map(mapToExecutionLog).list.apply()

  // ==================== マッピング関数 ====================

  private def mapToManagement(rs: WrappedResultSet): AutoJournalManagement =
    AutoJournalManagement(
      managementId = rs.string("自動仕訳管理ID"),
      name = rs.string("自動仕訳名"),
      sourceTable = rs.string("ソーステーブル名"),
      lastProcessedAt = rs.localDateTimeOpt("最終処理日時"),
      enabled = rs.int("有効フラグ") == 1,
      createdAt = rs.localDateTime("作成日時"),
      updatedAt = rs.localDateTime("更新日時"),
    )

  private def mapToPattern(rs: WrappedResultSet): AutoJournalPattern =
    AutoJournalPattern(
      patternId = rs.string("パターンID"),
      managementId = rs.string("自動仕訳管理ID"),
      patternName = rs.string("パターン名"),
      condition = rs.stringOpt("条件式"),
      priority = rs.int("優先順位"),
      enabled = rs.int("有効フラグ") == 1,
      createdAt = rs.localDateTime("作成日時"),
      updatedAt = rs.localDateTime("更新日時"),
    )

  private def mapToPatternDetail(rs: WrappedResultSet): AutoJournalPatternDetail =
    AutoJournalPatternDetail(
      patternId = rs.string("パターンID"),
      lineNumber = rs.int("行番号"),
      debitCreditType = DebitCreditType.fromCode(rs.string("貸借区分")),
      accountCode = rs.string("勘定科目コード"),
      amountExpression = rs.string("金額式"),
      descriptionTemplate = rs.stringOpt("摘要テンプレート"),
      createdAt = rs.localDateTime("作成日時"),
      updatedAt = rs.localDateTime("更新日時"),
    )

  private def mapToExecutionLog(rs: WrappedResultSet): AutoJournalExecutionLog =
    AutoJournalExecutionLog(
      logId = rs.long("実行ログID"),
      managementId = rs.string("自動仕訳管理ID"),
      startTime = rs.localDateTime("実行開始日時"),
      endTime = rs.localDateTimeOpt("実行終了日時"),
      totalCount = rs.int("処理件数"),
      successCount = rs.int("成功件数"),
      errorCount = rs.int("エラー件数"),
      result = ExecutionResult.fromCode(rs.string("実行結果")),
      errorMessage = rs.stringOpt("エラーメッセージ"),
      createdAt = rs.localDateTime("作成日時"),
    )
