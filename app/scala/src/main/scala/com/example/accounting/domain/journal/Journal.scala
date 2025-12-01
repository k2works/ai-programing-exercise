package com.example.accounting.domain.journal

import java.time.{LocalDate, LocalDateTime}

/**
 * 貸借区分
 */
enum DebitCreditType(val code: String):
  case Debit extends DebitCreditType("D")
  case Credit extends DebitCreditType("C")

object DebitCreditType:
  def fromCode(code: String): DebitCreditType =
    code match
      case "D" => Debit
      case "C" => Credit
      case _   => throw new IllegalArgumentException(s"Invalid debit/credit code: $code")

/**
 * 仕訳（3層構造のヘッダー）
 */
case class Journal(
    journalNo: String,
    journalDate: LocalDate,
    inputDate: LocalDate,
    settlementFlag: Int,
    singleEntryFlag: Int,
    journalType: Int,
    recurringFlag: Int,
    employeeCode: Option[String],
    departmentCode: Option[String],
    redSlipFlag: Int,
    redBlackVoucherNo: Option[String],
    createdAt: LocalDateTime,
    updatedAt: LocalDateTime,
):
  /** 決算仕訳かどうか */
  def isSettlement: Boolean = settlementFlag == 1

  /** 単一仕訳かどうか */
  def isSingleEntry: Boolean = singleEntryFlag == 1

  /** 赤伝票かどうか */
  def isRedSlip: Boolean = redSlipFlag == 1

/**
 * 仕訳明細（3層構造の明細）
 */
case class JournalDetail(
    journalNo: String,
    lineNumber: Int,
    lineDescription: String,
    createdAt: LocalDateTime,
    updatedAt: LocalDateTime,
)

/**
 * 仕訳貸借明細（3層構造の貸借明細）
 */
case class JournalDebitCreditDetail(
    journalNo: String,
    lineNumber: Int,
    debitCreditType: DebitCreditType,
    currencyCode: String,
    exchangeRate: BigDecimal,
    departmentCode: Option[String],
    projectCode: Option[String],
    accountCode: String,
    subAccountCode: Option[String],
    amount: BigDecimal,
    baseAmount: BigDecimal,
    taxType: Option[String],
    taxRate: Option[Int],
    taxCalculationType: Option[String],
    dueDate: Option[LocalDate],
    cashFlowFlag: Int,
    segmentCode: Option[String],
    counterAccountCode: Option[String],
    counterSubAccountCode: Option[String],
    memoCode: Option[String],
    memoContent: Option[String],
    createdAt: LocalDateTime,
    updatedAt: LocalDateTime,
):
  /** 借方かどうか */
  def isDebit: Boolean = debitCreditType == DebitCreditType.Debit

  /** 貸方かどうか */
  def isCredit: Boolean = debitCreditType == DebitCreditType.Credit

  /** 資金繰に影響するかどうか */
  def affectsCashFlow: Boolean = cashFlowFlag == 1
