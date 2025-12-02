package com.example.accounting.infrastructure.in.http.dto

import com.example.accounting.application.port.in.*
import com.example.accounting.domain.eventsourcing.*

import java.time.{Instant, LocalDate}

/**
 * 仕訳エントリ明細リクエスト DTO
 */
case class JournalEntryDetailRequest(
    lineNumber: Int,
    accountCode: String,
    debitCredit: String,
    amount: BigDecimal,
    description: Option[String],
)

/**
 * 仕訳エントリ作成リクエスト DTO
 */
case class JournalEntryCreateRequest(
    journalDate: LocalDate,
    description: String,
    details: List[JournalEntryDetailRequest],
    createdBy: String,
)

object JournalEntryCreateRequest:
  def toCommand(request: JournalEntryCreateRequest): CreateJournalEntryCommand =
    CreateJournalEntryCommand(
      journalDate = request.journalDate,
      description = request.description,
      details = request.details.map { d =>
        JournalDetailData(
          lineNumber = d.lineNumber,
          accountCode = d.accountCode,
          debitCredit = d.debitCredit,
          amount = d.amount,
          description = d.description,
        )
      },
      createdBy = request.createdBy,
    )

/**
 * 仕訳エントリ承認リクエスト DTO
 */
case class JournalEntryApproveRequest(
    approvedBy: String,
)

/**
 * 仕訳エントリ却下リクエスト DTO
 */
case class JournalEntryRejectRequest(
    rejectedBy: String,
    reason: String,
)

/**
 * 仕訳エントリ削除リクエスト DTO
 */
case class JournalEntryDeleteRequest(
    deletedBy: String,
    reason: String,
)

/**
 * 仕訳エントリ明細レスポンス DTO
 */
case class JournalEntryDetailResponse(
    lineNumber: Int,
    accountCode: String,
    debitCredit: String,
    amount: BigDecimal,
    description: Option[String],
)

object JournalEntryDetailResponse:
  def fromDomain(data: JournalDetailData): JournalEntryDetailResponse =
    JournalEntryDetailResponse(
      lineNumber = data.lineNumber,
      accountCode = data.accountCode,
      debitCredit = data.debitCredit,
      amount = data.amount,
      description = data.description,
    )

/**
 * 仕訳エントリレスポンス DTO
 */
case class JournalEntryResponse(
    id: String,
    version: Long,
    journalDate: Option[LocalDate],
    description: Option[String],
    details: List[JournalEntryDetailResponse],
    status: String,
    totalDebit: BigDecimal,
    totalCredit: BigDecimal,
    isBalanced: Boolean,
    createdBy: Option[String],
    createdAt: Option[Instant],
    approvedBy: Option[String],
    approvedAt: Option[Instant],
)

object JournalEntryResponse:
  def fromAggregate(aggregate: JournalEntryAggregate): JournalEntryResponse =
    JournalEntryResponse(
      id = aggregate.id,
      version = aggregate.version,
      journalDate = aggregate.journalDate,
      description = aggregate.description,
      details = aggregate.details.map(JournalEntryDetailResponse.fromDomain),
      status = aggregate.status.toString,
      totalDebit = aggregate.totalDebit,
      totalCredit = aggregate.totalCredit,
      isBalanced = aggregate.isBalanced,
      createdBy = aggregate.createdBy,
      createdAt = aggregate.createdAt,
      approvedBy = aggregate.approvedBy,
      approvedAt = aggregate.approvedAt,
    )

/**
 * 仕訳エントリイベントレスポンス DTO
 */
case class JournalEntryEventResponse(
    aggregateId: String,
    version: Long,
    eventType: String,
    timestamp: Instant,
)

object JournalEntryEventResponse:
  def fromEvent(event: JournalEntryEvent): JournalEntryEventResponse =
    JournalEntryEventResponse(
      aggregateId = event.aggregateId,
      version = event.version,
      eventType = event.getClass.getSimpleName,
      timestamp = event.timestamp,
    )
