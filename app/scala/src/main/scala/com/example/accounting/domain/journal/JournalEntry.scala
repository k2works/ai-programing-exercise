package com.example.accounting.domain.journal

import java.time.{LocalDate, LocalDateTime}

/**
 * 仕訳エントリ（2層構造のヘッダー）
 */
case class JournalEntry(
    voucherNumber: String,
    journalDate: LocalDate,
    description: String,
    totalAmount: BigDecimal,
    referenceNumber: Option[String],
    creator: String,
    updater: Option[String],
    createdAt: LocalDateTime,
    updatedAt: LocalDateTime,
)

/**
 * 仕訳明細（2層構造の明細）
 */
case class JournalEntryDetail(
    voucherNumber: String,
    lineNumber: Int,
    accountCode: String,
    debitAmount: BigDecimal,
    creditAmount: BigDecimal,
    description: String,
    taxAmount: Option[BigDecimal],
    taxRate: Option[BigDecimal],
)
