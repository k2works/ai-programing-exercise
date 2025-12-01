package com.example.accounting.infrastructure.http.dto

import com.example.accounting.domain.journal.*
import java.time.{LocalDate, LocalDateTime}

/**
 * 仕訳貸借明細リクエスト DTO
 */
case class JournalDebitCreditDetailRequest(
    lineNumber: Int,
    debitCreditType: String,
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
)

/**
 * 仕訳明細リクエスト DTO
 */
case class JournalDetailRequest(
    lineNumber: Int,
    lineDescription: String,
)

/**
 * 仕訳リクエスト DTO
 */
case class JournalRequest(
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
    details: List[JournalDetailRequest],
    debitCreditDetails: List[JournalDebitCreditDetailRequest],
)

object JournalRequest:
  def toDomain(request: JournalRequest): (Journal, List[JournalDetail], List[JournalDebitCreditDetail]) =
    val now = LocalDateTime.now()
    val journal = Journal(
      journalNo = request.journalNo,
      journalDate = request.journalDate,
      inputDate = request.inputDate,
      settlementFlag = request.settlementFlag,
      singleEntryFlag = request.singleEntryFlag,
      journalType = request.journalType,
      recurringFlag = request.recurringFlag,
      employeeCode = request.employeeCode,
      departmentCode = request.departmentCode,
      redSlipFlag = request.redSlipFlag,
      redBlackVoucherNo = request.redBlackVoucherNo,
      createdAt = now,
      updatedAt = now,
    )

    val details = request.details.map { d =>
      JournalDetail(
        journalNo = request.journalNo,
        lineNumber = d.lineNumber,
        lineDescription = d.lineDescription,
        createdAt = now,
        updatedAt = now,
      )
    }

    val debitCreditDetails = request.debitCreditDetails.map { d =>
      JournalDebitCreditDetail(
        journalNo = request.journalNo,
        lineNumber = d.lineNumber,
        debitCreditType = DebitCreditType.fromCode(d.debitCreditType),
        currencyCode = d.currencyCode,
        exchangeRate = d.exchangeRate,
        departmentCode = d.departmentCode,
        projectCode = d.projectCode,
        accountCode = d.accountCode,
        subAccountCode = d.subAccountCode,
        amount = d.amount,
        baseAmount = d.baseAmount,
        taxType = d.taxType,
        taxRate = d.taxRate,
        taxCalculationType = d.taxCalculationType,
        dueDate = d.dueDate,
        cashFlowFlag = d.cashFlowFlag,
        segmentCode = d.segmentCode,
        counterAccountCode = d.counterAccountCode,
        counterSubAccountCode = d.counterSubAccountCode,
        memoCode = d.memoCode,
        memoContent = d.memoContent,
        createdAt = now,
        updatedAt = now,
      )
    }

    (journal, details, debitCreditDetails)

/**
 * 仕訳貸借明細レスポンス DTO
 */
case class JournalDebitCreditDetailResponse(
    journalNo: String,
    lineNumber: Int,
    debitCreditType: String,
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
)

object JournalDebitCreditDetailResponse:
  def fromDomain(detail: JournalDebitCreditDetail): JournalDebitCreditDetailResponse =
    JournalDebitCreditDetailResponse(
      journalNo = detail.journalNo,
      lineNumber = detail.lineNumber,
      debitCreditType = detail.debitCreditType.code,
      currencyCode = detail.currencyCode,
      exchangeRate = detail.exchangeRate,
      departmentCode = detail.departmentCode,
      projectCode = detail.projectCode,
      accountCode = detail.accountCode,
      subAccountCode = detail.subAccountCode,
      amount = detail.amount,
      baseAmount = detail.baseAmount,
      taxType = detail.taxType,
      taxRate = detail.taxRate,
      taxCalculationType = detail.taxCalculationType,
      dueDate = detail.dueDate,
      cashFlowFlag = detail.cashFlowFlag,
      segmentCode = detail.segmentCode,
      counterAccountCode = detail.counterAccountCode,
      counterSubAccountCode = detail.counterSubAccountCode,
      memoCode = detail.memoCode,
      memoContent = detail.memoContent,
    )

/**
 * 仕訳明細レスポンス DTO
 */
case class JournalDetailResponse(
    journalNo: String,
    lineNumber: Int,
    lineDescription: String,
)

object JournalDetailResponse:
  def fromDomain(detail: JournalDetail): JournalDetailResponse =
    JournalDetailResponse(
      journalNo = detail.journalNo,
      lineNumber = detail.lineNumber,
      lineDescription = detail.lineDescription,
    )

/**
 * 仕訳レスポンス DTO
 */
case class JournalResponse(
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
    details: List[JournalDetailResponse],
    debitCreditDetails: List[JournalDebitCreditDetailResponse],
    totalDebit: BigDecimal,
    totalCredit: BigDecimal,
    isBalanced: Boolean,
)

object JournalResponse:
  def fromDomain(
      journal: Journal,
      details: List[JournalDetail],
      debitCreditDetails: List[JournalDebitCreditDetail],
  ): JournalResponse =
    val totalDebit = debitCreditDetails.filter(_.isDebit).map(_.amount).sum
    val totalCredit = debitCreditDetails.filter(_.isCredit).map(_.amount).sum
    JournalResponse(
      journalNo = journal.journalNo,
      journalDate = journal.journalDate,
      inputDate = journal.inputDate,
      settlementFlag = journal.settlementFlag,
      singleEntryFlag = journal.singleEntryFlag,
      journalType = journal.journalType,
      recurringFlag = journal.recurringFlag,
      employeeCode = journal.employeeCode,
      departmentCode = journal.departmentCode,
      redSlipFlag = journal.redSlipFlag,
      redBlackVoucherNo = journal.redBlackVoucherNo,
      details = details.map(JournalDetailResponse.fromDomain),
      debitCreditDetails = debitCreditDetails.map(JournalDebitCreditDetailResponse.fromDomain),
      totalDebit = totalDebit,
      totalCredit = totalCredit,
      isBalanced = totalDebit == totalCredit,
    )

/**
 * 仕訳貸借検証レスポンス DTO
 */
case class JournalValidationResponse(
    journalNo: String,
    isBalanced: Boolean,
    totalDebit: BigDecimal,
    totalCredit: BigDecimal,
)
