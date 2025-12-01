package com.example.accounting.infrastructure.journal

import com.example.accounting.domain.journal.*
import scalikejdbc.*

import java.time.{LocalDate, LocalDateTime}

/**
 * 仕訳リポジトリ（3層構造）
 */
class JournalRepository:

  /**
   * 仕訳を登録
   */
  def insertJournal(journal: Journal)(implicit session: DBSession): Int =
    sql"""
      INSERT INTO "仕訳" (
        "仕訳伝票番号", "起票日", "入力日", "決算仕訳フラグ", "単振フラグ",
        "仕訳伝票区分", "定期計上フラグ", "社員コード", "部門コード",
        "赤伝フラグ", "赤黒伝票番号"
      ) VALUES (
        ${journal.journalNo}, ${journal.journalDate}, ${journal.inputDate},
        ${journal.settlementFlag}, ${journal.singleEntryFlag}, ${journal.journalType},
        ${journal.recurringFlag}, ${journal.employeeCode}, ${journal.departmentCode},
        ${journal.redSlipFlag}, ${journal.redBlackVoucherNo}
      )
    """.update.apply()

  /**
   * 仕訳明細を登録
   */
  def insertDetail(detail: JournalDetail)(implicit session: DBSession): Int =
    sql"""
      INSERT INTO "仕訳明細3" (
        "仕訳伝票番号", "仕訳行番号", "行摘要"
      ) VALUES (
        ${detail.journalNo}, ${detail.lineNumber}, ${detail.lineDescription}
      )
    """.update.apply()

  /**
   * 仕訳貸借明細を登録
   */
  def insertDebitCreditDetail(detail: JournalDebitCreditDetail)(implicit session: DBSession): Int =
    sql"""
      INSERT INTO "仕訳貸借明細" (
        "仕訳伝票番号", "仕訳行番号", "仕訳行貸借区分",
        "通貨コード", "為替レート", "部門コード", "プロジェクトコード",
        "勘定科目コード", "補助科目コード", "仕訳金額", "基軸換算仕訳金額",
        "消費税区分", "消費税率", "消費税計算区分", "期日", "資金繰フラグ",
        "セグメントコード", "相手勘定科目コード", "相手補助科目コード",
        "付箋コード", "付箋内容"
      ) VALUES (
        ${detail.journalNo}, ${detail.lineNumber}, ${detail.debitCreditType.code},
        ${detail.currencyCode}, ${detail.exchangeRate}, ${detail.departmentCode},
        ${detail.projectCode}, ${detail.accountCode}, ${detail.subAccountCode},
        ${detail.amount}, ${detail.baseAmount}, ${detail.taxType}, ${detail.taxRate},
        ${detail.taxCalculationType}, ${detail.dueDate}, ${detail.cashFlowFlag},
        ${detail.segmentCode}, ${detail.counterAccountCode}, ${detail.counterSubAccountCode},
        ${detail.memoCode}, ${detail.memoContent}
      )
    """.update.apply()

  /**
   * 仕訳伝票番号で検索
   */
  def findByNo(journalNo: String)(implicit session: DBSession): Option[Journal] =
    sql"""
      SELECT * FROM "仕訳" WHERE "仕訳伝票番号" = ${journalNo}
    """.map(mapToJournal).single.apply()

  /**
   * 起票日の範囲で検索
   */
  def findByDateRange(from: LocalDate, to: LocalDate)(implicit session: DBSession): List[Journal] =
    sql"""
      SELECT * FROM "仕訳"
      WHERE "起票日" >= ${from} AND "起票日" <= ${to}
      ORDER BY "起票日", "仕訳伝票番号"
    """.map(mapToJournal).list.apply()

  /**
   * 決算仕訳のみ取得
   */
  def findSettlementJournals()(implicit session: DBSession): List[Journal] =
    sql"""
      SELECT * FROM "仕訳"
      WHERE "決算仕訳フラグ" = 1
      ORDER BY "起票日", "仕訳伝票番号"
    """.map(mapToJournal).list.apply()

  /**
   * 仕訳明細を取得
   */
  def findDetailsByNo(journalNo: String)(implicit session: DBSession): List[JournalDetail] =
    sql"""
      SELECT * FROM "仕訳明細3"
      WHERE "仕訳伝票番号" = ${journalNo}
      ORDER BY "仕訳行番号"
    """.map(mapToDetail).list.apply()

  /**
   * 仕訳貸借明細を取得
   */
  def findDebitCreditDetailsByNo(journalNo: String)(implicit session: DBSession): List[JournalDebitCreditDetail] =
    sql"""
      SELECT * FROM "仕訳貸借明細"
      WHERE "仕訳伝票番号" = ${journalNo}
      ORDER BY "仕訳行番号", "仕訳行貸借区分"
    """.map(mapToDebitCreditDetail).list.apply()

  /**
   * 仕訳を削除
   */
  def delete(journalNo: String)(implicit session: DBSession): Int =
    sql"""DELETE FROM "仕訳" WHERE "仕訳伝票番号" = ${journalNo}""".update.apply()

  /**
   * 貸借平衡を検証
   */
  def validateBalance(journalNo: String)(implicit session: DBSession): Option[(Boolean, BigDecimal, BigDecimal)] =
    sql"""
      SELECT * FROM validate_journal_balance(${journalNo})
    """.map { rs =>
      (
        rs.boolean("is_balanced"),
        BigDecimal(rs.bigDecimal("debit_total")),
        BigDecimal(rs.bigDecimal("credit_total")),
      )
    }.single.apply()

  private def mapToJournal(rs: WrappedResultSet): Journal =
    Journal(
      journalNo = rs.string("仕訳伝票番号"),
      journalDate = rs.localDate("起票日"),
      inputDate = rs.localDate("入力日"),
      settlementFlag = rs.int("決算仕訳フラグ"),
      singleEntryFlag = rs.int("単振フラグ"),
      journalType = rs.int("仕訳伝票区分"),
      recurringFlag = rs.int("定期計上フラグ"),
      employeeCode = rs.stringOpt("社員コード"),
      departmentCode = rs.stringOpt("部門コード"),
      redSlipFlag = rs.int("赤伝フラグ"),
      redBlackVoucherNo = rs.stringOpt("赤黒伝票番号"),
      createdAt = rs.localDateTime("作成日時"),
      updatedAt = rs.localDateTime("更新日時"),
    )

  private def mapToDetail(rs: WrappedResultSet): JournalDetail =
    JournalDetail(
      journalNo = rs.string("仕訳伝票番号"),
      lineNumber = rs.int("仕訳行番号"),
      lineDescription = rs.string("行摘要"),
      createdAt = rs.localDateTime("作成日時"),
      updatedAt = rs.localDateTime("更新日時"),
    )

  private def mapToDebitCreditDetail(rs: WrappedResultSet): JournalDebitCreditDetail =
    JournalDebitCreditDetail(
      journalNo = rs.string("仕訳伝票番号"),
      lineNumber = rs.int("仕訳行番号"),
      debitCreditType = DebitCreditType.fromCode(rs.string("仕訳行貸借区分")),
      currencyCode = rs.string("通貨コード"),
      exchangeRate = BigDecimal(rs.bigDecimal("為替レート")),
      departmentCode = rs.stringOpt("部門コード"),
      projectCode = rs.stringOpt("プロジェクトコード"),
      accountCode = rs.string("勘定科目コード"),
      subAccountCode = rs.stringOpt("補助科目コード"),
      amount = BigDecimal(rs.bigDecimal("仕訳金額")),
      baseAmount = BigDecimal(rs.bigDecimal("基軸換算仕訳金額")),
      taxType = rs.stringOpt("消費税区分"),
      taxRate = rs.intOpt("消費税率"),
      taxCalculationType = rs.stringOpt("消費税計算区分"),
      dueDate = rs.localDateOpt("期日"),
      cashFlowFlag = rs.int("資金繰フラグ"),
      segmentCode = rs.stringOpt("セグメントコード"),
      counterAccountCode = rs.stringOpt("相手勘定科目コード"),
      counterSubAccountCode = rs.stringOpt("相手補助科目コード"),
      memoCode = rs.stringOpt("付箋コード"),
      memoContent = rs.stringOpt("付箋内容"),
      createdAt = rs.localDateTime("作成日時"),
      updatedAt = rs.localDateTime("更新日時"),
    )
