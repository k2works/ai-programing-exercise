package com.example.financial.domain

import java.time.{LocalDate, LocalDateTime}
import com.example.common.domain.*

/**
 * 仕訳エンティティ
 *
 * 財務会計コンテキストにおける仕訳を表現します。
 * 仕訳は複式簿記に基づく取引記録であり、
 * 借方と貸方の金額が必ず一致する必要があります。
 */
case class Journal(
    journalId: Option[Long],
    journalDate: LocalDate,
    description: String,
    fiscalYear: FiscalYear,
    entries: List[JournalEntry],
    createdAt: LocalDateTime,
    updatedAt: LocalDateTime
)

object Journal:
  /**
   * 新規仕訳を作成
   */
  def create(
      journalDate: LocalDate,
      description: String,
      fiscalYear: FiscalYear,
      entries: List[JournalEntry]
  ): Either[String, Journal] =
    validate(entries).map { validEntries =>
      val now = LocalDateTime.now()
      Journal(
        journalId = None,
        journalDate = journalDate,
        description = description,
        fiscalYear = fiscalYear,
        entries = validEntries,
        createdAt = now,
        updatedAt = now
      )
    }

  /**
   * 仕訳明細のバリデーション
   * - 借方合計と貸方合計が一致すること
   * - 少なくとも1つの借方明細と1つの貸方明細があること
   */
  private def validate(entries: List[JournalEntry]): Either[String, List[JournalEntry]] =
    if entries.isEmpty then Left("仕訳明細が必要です")
    else
      val debitTotal = entries.map(_.debitAmount).foldLeft(Money.Zero)(_ + _)
      val creditTotal = entries.map(_.creditAmount).foldLeft(Money.Zero)(_ + _)

      if debitTotal.value != creditTotal.value then
        Left(s"借方合計(${debitTotal.value})と貸方合計(${creditTotal.value})が一致しません")
      else if debitTotal.isZero then Left("借方合計が0です")
      else if entries.forall(_.debitAmount.isZero) then Left("借方明細が必要です")
      else if entries.forall(_.creditAmount.isZero) then Left("貸方明細が必要です")
      else Right(entries)

/**
 * 仕訳明細エンティティ
 */
case class JournalEntry(
    entryId: Option[Long],
    journalId: Option[Long],
    accountCode: AccountCode,
    debitAmount: Money,
    creditAmount: Money,
    description: String
)

object JournalEntry:
  /**
   * 借方明細を作成
   */
  def debit(accountCode: AccountCode, amount: Money, description: String = ""): JournalEntry =
    JournalEntry(
      entryId = None,
      journalId = None,
      accountCode = accountCode,
      debitAmount = amount,
      creditAmount = Money.Zero,
      description = description
    )

  /**
   * 貸方明細を作成
   */
  def credit(accountCode: AccountCode, amount: Money, description: String = ""): JournalEntry =
    JournalEntry(
      entryId = None,
      journalId = None,
      accountCode = accountCode,
      debitAmount = Money.Zero,
      creditAmount = amount,
      description = description
    )
