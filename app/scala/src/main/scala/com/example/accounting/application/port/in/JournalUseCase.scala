package com.example.accounting.application.port.in

import com.example.accounting.application.AppError
import com.example.accounting.domain.journal.*

import java.time.LocalDate

/**
 * 仕訳ユースケース（Input Port）
 */
trait JournalUseCase:

  /** 仕訳伝票番号で取得 */
  def getJournal(journalNo: String): Either[AppError, (Journal, List[JournalDetail], List[JournalDebitCreditDetail])]

  /** 日付範囲で仕訳を取得 */
  def getJournalsByDateRange(
      from: LocalDate,
      to: LocalDate,
  ): Either[AppError, List[(Journal, List[JournalDetail], List[JournalDebitCreditDetail])]]

  /** 仕訳を作成 */
  def createJournal(
      journal: Journal,
      details: List[JournalDetail],
      debitCreditDetails: List[JournalDebitCreditDetail],
  ): Either[AppError, (Journal, List[JournalDetail], List[JournalDebitCreditDetail])]

  /** 仕訳を削除 */
  def deleteJournal(journalNo: String): Either[AppError, Unit]

  /** 貸借平衡を検証 */
  def validateBalance(journalNo: String): Either[AppError, (Boolean, BigDecimal, BigDecimal)]
