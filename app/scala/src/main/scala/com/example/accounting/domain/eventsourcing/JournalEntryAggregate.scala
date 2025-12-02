package com.example.accounting.domain.eventsourcing

import java.time.{Instant, LocalDate}

/**
 * 仕訳の状態
 */
enum JournalStatus:
  case Draft
  case Approved
  case Rejected
  case Deleted

/**
 * 仕訳 Aggregate（完全に不変）
 *
 * イベントソーシングにおける Aggregate は、イベントを適用して状態を管理するドメインオブジェクトです。
 */
case class JournalEntryAggregate(
    id: String,
    version: Long,
    journalDate: Option[LocalDate],
    description: Option[String],
    details: List[JournalDetailData],
    status: JournalStatus,
    createdBy: Option[String],
    createdAt: Option[Instant],
    approvedBy: Option[String],
    approvedAt: Option[Instant],
):

  /**
   * イベントを適用して新しい状態を返す
   */
  def apply(event: JournalEntryEvent): JournalEntryAggregate = event match
    case e: JournalEntryCreatedEvent =>
      this.copy(
        version = e.version,
        journalDate = Some(e.journalDate),
        description = Some(e.description),
        details = e.details,
        status = JournalStatus.Draft,
        createdBy = Some(e.createdBy),
        createdAt = Some(e.timestamp),
      )

    case e: JournalEntryApprovedEvent =>
      this.copy(
        version = e.version,
        status = JournalStatus.Approved,
        approvedBy = Some(e.approvedBy),
        approvedAt = Some(e.approvedAt),
      )

    case e: JournalEntryRejectedEvent =>
      this.copy(
        version = e.version,
        status = JournalStatus.Rejected,
      )

    case e: JournalEntryDeletedEvent =>
      this.copy(
        version = e.version,
        status = JournalStatus.Deleted,
      )

  /**
   * 承認可能か判定
   */
  def canApprove: Boolean = status == JournalStatus.Draft

  /**
   * 却下可能か判定
   */
  def canReject: Boolean = status == JournalStatus.Draft

  /**
   * 削除可能か判定
   */
  def canDelete: Boolean = status == JournalStatus.Draft || status == JournalStatus.Approved

  /**
   * 借方合計を計算
   */
  def totalDebit: BigDecimal =
    details.filter(_.debitCredit == "D").map(_.amount).sum

  /**
   * 貸方合計を計算
   */
  def totalCredit: BigDecimal =
    details.filter(_.debitCredit == "C").map(_.amount).sum

  /**
   * 貸借一致しているか判定
   */
  def isBalanced: Boolean = totalDebit == totalCredit

object JournalEntryAggregate:

  /**
   * 空の Aggregate（初期状態）
   */
  def empty(id: String): JournalEntryAggregate =
    JournalEntryAggregate(
      id = id,
      version = 0,
      journalDate = None,
      description = None,
      details = List.empty,
      status = JournalStatus.Draft,
      createdBy = None,
      createdAt = None,
      approvedBy = None,
      approvedAt = None,
    )

  /**
   * イベント再生
   *
   * foldLeft を使って、イベントのリストから状態を復元します。
   */
  def replay(events: List[JournalEntryEvent]): Either[String, JournalEntryAggregate] =
    events.headOption match
      case Some(firstEvent) =>
        val initial = empty(firstEvent.aggregateId)
        Right(events.foldLeft(initial) { (aggregate, event) =>
          aggregate.apply(event)
        })
      case None =>
        Left("イベントが空です")

  /**
   * スナップショットから復元し、差分イベントを適用
   */
  def replayFromSnapshot(
      snapshot: JournalEntryAggregate,
      newEvents: List[JournalEntryEvent],
  ): JournalEntryAggregate =
    newEvents.foldLeft(snapshot) { (aggregate, event) =>
      aggregate.apply(event)
    }
