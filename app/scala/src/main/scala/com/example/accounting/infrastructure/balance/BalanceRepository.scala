package com.example.accounting.infrastructure.balance

import com.example.accounting.domain.balance.*
import scalikejdbc.*

import java.time.{LocalDate, LocalDateTime}

/**
 * 残高リポジトリ
 */
class BalanceRepository:

  // ==================== 日次残高 ====================

  /**
   * 日次残高を更新（UPSERT）
   */
  def upsertDailyBalance(
      entryDate: LocalDate,
      accountCode: String,
      subAccountCode: String,
      departmentCode: String,
      projectCode: String,
      settlementFlag: Int,
      debitAmount: BigDecimal,
      creditAmount: BigDecimal,
  )(implicit session: DBSession): Unit =
    val safeSubAccountCode = Option(subAccountCode).getOrElse("")
    val safeDepartmentCode = Option(departmentCode).getOrElse("")
    val safeProjectCode = Option(projectCode).getOrElse("")
    val safeSettlementFlag = Option(settlementFlag).getOrElse(0)

    sql"""
      INSERT INTO "日次勘定科目残高" (
        "起票日", "勘定科目コード", "補助科目コード", "部門コード",
        "プロジェクトコード", "決算仕訳フラグ", "借方金額", "貸方金額"
      ) VALUES (
        ${entryDate}, ${accountCode}, ${safeSubAccountCode}, ${safeDepartmentCode},
        ${safeProjectCode}, ${safeSettlementFlag}, ${debitAmount}, ${creditAmount}
      )
      ON CONFLICT (
        "起票日", "勘定科目コード", "補助科目コード", "部門コード",
        "プロジェクトコード", "決算仕訳フラグ"
      )
      DO UPDATE SET
        "借方金額" = "日次勘定科目残高"."借方金額" + EXCLUDED."借方金額",
        "貸方金額" = "日次勘定科目残高"."貸方金額" + EXCLUDED."貸方金額",
        "更新日時" = CURRENT_TIMESTAMP
    """.update.apply()

  /**
   * 日次残高を一括更新（バッチ処理）
   */
  def upsertDailyBalanceBatch(records: List[DailyBalanceUpdateRecord])(implicit session: DBSession): Unit =
    records.foreach { record =>
      upsertDailyBalance(
        record.entryDate,
        record.accountCode,
        record.subAccountCode,
        record.departmentCode,
        record.projectCode,
        record.settlementFlag,
        record.debitAmount,
        record.creditAmount,
      )
    }

  /**
   * 日次残高を取得
   */
  def findDailyBalance(key: DailyAccountBalanceKey)(implicit session: DBSession): Option[DailyAccountBalance] =
    sql"""
      SELECT * FROM "日次勘定科目残高"
      WHERE "起票日" = ${key.entryDate}
        AND "勘定科目コード" = ${key.accountCode}
        AND "補助科目コード" = ${key.subAccountCode}
        AND "部門コード" = ${key.departmentCode}
        AND "プロジェクトコード" = ${key.projectCode}
        AND "決算仕訳フラグ" = ${key.settlementFlag}
    """.map(mapToDailyBalance).single.apply()

  /**
   * 日付範囲で日次残高を取得
   */
  def findDailyBalancesByDateRange(
      from: LocalDate,
      to: LocalDate,
  )(implicit session: DBSession): List[DailyAccountBalance] =
    sql"""
      SELECT * FROM "日次勘定科目残高"
      WHERE "起票日" >= ${from} AND "起票日" <= ${to}
      ORDER BY "起票日", "勘定科目コード"
    """.map(mapToDailyBalance).list.apply()

  /**
   * 勘定科目別の日次残高を取得
   */
  def findDailyBalancesByAccount(accountCode: String)(implicit session: DBSession): List[DailyAccountBalance] =
    sql"""
      SELECT * FROM "日次勘定科目残高"
      WHERE "勘定科目コード" = ${accountCode}
      ORDER BY "起票日"
    """.map(mapToDailyBalance).list.apply()

  /**
   * 部門別の日次残高を取得
   */
  def findDailyBalancesByDepartment(departmentCode: String)(implicit session: DBSession): List[DailyAccountBalance] =
    sql"""
      SELECT * FROM "日次勘定科目残高"
      WHERE "部門コード" = ${departmentCode}
      ORDER BY "起票日", "勘定科目コード"
    """.map(mapToDailyBalance).list.apply()

  /**
   * 日次残高を削除
   */
  def deleteDailyBalance(key: DailyAccountBalanceKey)(implicit session: DBSession): Int =
    sql"""
      DELETE FROM "日次勘定科目残高"
      WHERE "起票日" = ${key.entryDate}
        AND "勘定科目コード" = ${key.accountCode}
        AND "補助科目コード" = ${key.subAccountCode}
        AND "部門コード" = ${key.departmentCode}
        AND "プロジェクトコード" = ${key.projectCode}
        AND "決算仕訳フラグ" = ${key.settlementFlag}
    """.update.apply()

  // ==================== 月次残高 ====================

  /**
   * 月次残高を登録/更新
   */
  def upsertMonthlyBalance(balance: MonthlyAccountBalance)(implicit session: DBSession): Unit =
    sql"""
      INSERT INTO "月次勘定科目残高" (
        "決算期", "月度", "勘定科目コード", "補助科目コード",
        "部門コード", "プロジェクトコード", "決算仕訳フラグ",
        "月初残高", "借方金額", "貸方金額", "月末残高"
      ) VALUES (
        ${balance.fiscalYear}, ${balance.month}, ${balance.accountCode},
        ${balance.subAccountCode}, ${balance.departmentCode}, ${balance.projectCode},
        ${balance.settlementFlag}, ${balance.openingBalance},
        ${balance.debitAmount}, ${balance.creditAmount}, ${balance.closingBalance}
      )
      ON CONFLICT (
        "決算期", "月度", "勘定科目コード", "補助科目コード",
        "部門コード", "プロジェクトコード", "決算仕訳フラグ"
      )
      DO UPDATE SET
        "月初残高" = EXCLUDED."月初残高",
        "借方金額" = EXCLUDED."借方金額",
        "貸方金額" = EXCLUDED."貸方金額",
        "月末残高" = EXCLUDED."月末残高",
        "更新日時" = CURRENT_TIMESTAMP
    """.update.apply()

  /**
   * 月次残高を取得
   */
  def findMonthlyBalance(key: MonthlyAccountBalanceKey)(implicit session: DBSession): Option[MonthlyAccountBalance] =
    sql"""
      SELECT * FROM "月次勘定科目残高"
      WHERE "決算期" = ${key.fiscalYear}
        AND "月度" = ${key.month}
        AND "勘定科目コード" = ${key.accountCode}
        AND "補助科目コード" = ${key.subAccountCode}
        AND "部門コード" = ${key.departmentCode}
        AND "プロジェクトコード" = ${key.projectCode}
        AND "決算仕訳フラグ" = ${key.settlementFlag}
    """.map(mapToMonthlyBalance).single.apply()

  /**
   * 決算期の月次残高を取得
   */
  def findMonthlyBalancesByFiscalYear(fiscalYear: Int)(implicit session: DBSession): List[MonthlyAccountBalance] =
    sql"""
      SELECT * FROM "月次勘定科目残高"
      WHERE "決算期" = ${fiscalYear}
      ORDER BY "月度", "勘定科目コード"
    """.map(mapToMonthlyBalance).list.apply()

  /**
   * 月次締め処理（日次残高から月次残高を生成）
   */
  def closeMonth(fiscalYear: Int, month: Int)(implicit session: DBSession): Int =
    sql"""SELECT close_monthly_balance(${fiscalYear}, ${month})"""
      .map(_.int(1))
      .single
      .apply()
      .getOrElse(0)

  // ==================== マッピング関数 ====================

  private def mapToDailyBalance(rs: WrappedResultSet): DailyAccountBalance =
    DailyAccountBalance(
      entryDate = rs.localDate("起票日"),
      accountCode = rs.string("勘定科目コード"),
      subAccountCode = rs.string("補助科目コード"),
      departmentCode = rs.string("部門コード"),
      projectCode = rs.string("プロジェクトコード"),
      settlementFlag = rs.int("決算仕訳フラグ"),
      debitAmount = BigDecimal(rs.bigDecimal("借方金額")),
      creditAmount = BigDecimal(rs.bigDecimal("貸方金額")),
      createdAt = rs.localDateTime("作成日時"),
      updatedAt = rs.localDateTime("更新日時"),
    )

  private def mapToMonthlyBalance(rs: WrappedResultSet): MonthlyAccountBalance =
    MonthlyAccountBalance(
      fiscalYear = rs.int("決算期"),
      month = rs.int("月度"),
      accountCode = rs.string("勘定科目コード"),
      subAccountCode = rs.string("補助科目コード"),
      departmentCode = rs.string("部門コード"),
      projectCode = rs.string("プロジェクトコード"),
      settlementFlag = rs.int("決算仕訳フラグ"),
      openingBalance = BigDecimal(rs.bigDecimal("月初残高")),
      debitAmount = BigDecimal(rs.bigDecimal("借方金額")),
      creditAmount = BigDecimal(rs.bigDecimal("貸方金額")),
      closingBalance = BigDecimal(rs.bigDecimal("月末残高")),
      createdAt = rs.localDateTime("作成日時"),
      updatedAt = rs.localDateTime("更新日時"),
    )
