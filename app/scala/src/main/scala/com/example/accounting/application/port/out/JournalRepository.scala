package com.example.accounting.application.port.out

import com.example.accounting.domain.journal.*
import scalikejdbc.DBSession

import java.time.LocalDate

/**
 * 仕訳リポジトリ（Output Port）
 */
trait JournalRepository:

  /** 仕訳を登録 */
  def insertJournal(journal: Journal)(implicit session: DBSession): Int

  /** 仕訳明細を登録 */
  def insertDetail(detail: JournalDetail)(implicit session: DBSession): Int

  /** 仕訳貸借明細を登録 */
  def insertDebitCreditDetail(detail: JournalDebitCreditDetail)(implicit session: DBSession): Int

  /** 仕訳伝票番号で検索 */
  def findByNo(journalNo: String)(implicit session: DBSession): Option[Journal]

  /** 起票日の範囲で検索 */
  def findByDateRange(from: LocalDate, to: LocalDate)(implicit session: DBSession): List[Journal]

  /** 決算仕訳のみ取得 */
  def findSettlementJournals()(implicit session: DBSession): List[Journal]

  /** 仕訳明細を取得 */
  def findDetailsByNo(journalNo: String)(implicit session: DBSession): List[JournalDetail]

  /** 仕訳貸借明細を取得 */
  def findDebitCreditDetailsByNo(journalNo: String)(implicit session: DBSession): List[JournalDebitCreditDetail]

  /** 仕訳を削除 */
  def delete(journalNo: String)(implicit session: DBSession): Int

  /** 貸借平衡を検証 */
  def validateBalance(journalNo: String)(implicit session: DBSession): Option[(Boolean, BigDecimal, BigDecimal)]
