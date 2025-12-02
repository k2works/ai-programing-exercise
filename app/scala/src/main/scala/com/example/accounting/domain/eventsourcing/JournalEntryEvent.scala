package com.example.accounting.domain.eventsourcing

import java.time.{Instant, LocalDate}

/**
 * 仕訳明細（イベントソーシング用）
 */
case class JournalDetailData(
    lineNumber: Int,
    accountCode: String,
    debitCredit: String,
    amount: BigDecimal,
    description: Option[String],
)

/**
 * 仕訳イベントの基底 trait
 */
sealed trait JournalEntryEvent:
  def aggregateId: String
  def version: Long
  def timestamp: Instant

/**
 * 仕訳作成イベント
 */
case class JournalEntryCreatedEvent(
    aggregateId: String,
    version: Long,
    timestamp: Instant,
    journalDate: LocalDate,
    description: String,
    details: List[JournalDetailData],
    createdBy: String,
) extends JournalEntryEvent

/**
 * 仕訳承認イベント
 */
case class JournalEntryApprovedEvent(
    aggregateId: String,
    version: Long,
    timestamp: Instant,
    approvedBy: String,
    approvedAt: Instant,
) extends JournalEntryEvent

/**
 * 仕訳却下イベント
 */
case class JournalEntryRejectedEvent(
    aggregateId: String,
    version: Long,
    timestamp: Instant,
    rejectedBy: String,
    reason: String,
) extends JournalEntryEvent

/**
 * 仕訳削除イベント
 */
case class JournalEntryDeletedEvent(
    aggregateId: String,
    version: Long,
    timestamp: Instant,
    deletedBy: String,
    reason: String,
) extends JournalEntryEvent
