package com.example.accounting.infrastructure.in.http.dto

import com.example.accounting.domain.account.{Account, AccountType}
import java.time.LocalDateTime

/**
 * 勘定科目リクエスト DTO
 */
case class AccountRequest(
    accountCode: String,
    accountName: String,
    accountType: String,
    balance: BigDecimal,
    bsplDistinction: Option[String],
    transactionElement: Option[String],
    expenseDistinction: Option[String],
    isSummaryAccount: Boolean,
    displayOrder: Option[Int],
    isAggregationTarget: Boolean,
    accountKana: Option[String],
    taxCode: Option[String],
)

object AccountRequest:
  def toDomain(request: AccountRequest): Account =
    val now = LocalDateTime.now()
    Account(
      accountId = None,
      accountCode = request.accountCode,
      accountName = request.accountName,
      accountType = AccountType.fromString(request.accountType),
      balance = request.balance,
      bsplDistinction = request.bsplDistinction,
      transactionElement = request.transactionElement,
      expenseDistinction = request.expenseDistinction,
      isSummaryAccount = request.isSummaryAccount,
      displayOrder = request.displayOrder,
      isAggregationTarget = request.isAggregationTarget,
      accountKana = request.accountKana,
      taxCode = request.taxCode,
      createdAt = now,
      updatedAt = now,
    )

/**
 * 勘定科目レスポンス DTO
 */
case class AccountResponse(
    accountId: Option[Int],
    accountCode: String,
    accountName: String,
    accountType: String,
    balance: BigDecimal,
    bsplDistinction: Option[String],
    transactionElement: Option[String],
    expenseDistinction: Option[String],
    isSummaryAccount: Boolean,
    displayOrder: Option[Int],
    isAggregationTarget: Boolean,
    accountKana: Option[String],
    taxCode: Option[String],
)

object AccountResponse:
  def fromDomain(account: Account): AccountResponse =
    AccountResponse(
      accountId = account.accountId,
      accountCode = account.accountCode,
      accountName = account.accountName,
      accountType = account.accountType.value,
      balance = account.balance,
      bsplDistinction = account.bsplDistinction,
      transactionElement = account.transactionElement,
      expenseDistinction = account.expenseDistinction,
      isSummaryAccount = account.isSummaryAccount,
      displayOrder = account.displayOrder,
      isAggregationTarget = account.isAggregationTarget,
      accountKana = account.accountKana,
      taxCode = account.taxCode,
    )
