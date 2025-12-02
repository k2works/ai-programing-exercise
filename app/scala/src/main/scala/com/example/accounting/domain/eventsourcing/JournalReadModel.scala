package com.example.accounting.domain.eventsourcing

import java.time.{Instant, LocalDate}

/**
 * 仕訳リードモデル（CQRS 用）
 *
 * イベントソーシングの Write Model から Projection によって生成される
 * 読み取り専用のモデル
 */
case class JournalReadModel(
    aggregateId: String,
    journalDate: LocalDate,
    description: String,
    totalDebit: BigDecimal,
    totalCredit: BigDecimal,
    status: String,
    createdAt: Instant,
    approvedAt: Option[Instant],
    approvedBy: Option[String],
    version: Long,
)
