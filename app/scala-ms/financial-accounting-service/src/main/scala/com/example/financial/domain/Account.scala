package com.example.financial.domain

import java.time.LocalDateTime
import com.example.common.domain.*

/**
 * 勘定科目エンティティ
 *
 * 財務会計コンテキストにおける勘定科目を表現します。
 * 勘定科目は取引を分類するための科目であり、
 * 複式簿記の基本的な構成要素です。
 */
case class Account(
    accountId: Option[Long],
    accountCode: AccountCode,
    accountName: String,
    accountType: AccountType,
    isSummaryAccount: Boolean,
    displayOrder: Int,
    isAggregationTarget: Boolean,
    balance: Money,
    createdAt: LocalDateTime,
    updatedAt: LocalDateTime
)

object Account:
  /**
   * 新規勘定科目を作成
   */
  def create(
      accountCode: AccountCode,
      accountName: String,
      accountType: AccountType,
      isSummaryAccount: Boolean = false,
      displayOrder: Int = 0,
      isAggregationTarget: Boolean = true
  ): Account =
    val now = LocalDateTime.now()
    Account(
      accountId = None,
      accountCode = accountCode,
      accountName = accountName,
      accountType = accountType,
      isSummaryAccount = isSummaryAccount,
      displayOrder = displayOrder,
      isAggregationTarget = isAggregationTarget,
      balance = Money.Zero,
      createdAt = now,
      updatedAt = now
    )

  /**
   * 勘定科目の残高を更新
   */
  extension (account: Account)
    def updateBalance(newBalance: Money): Account =
      account.copy(
        balance = newBalance,
        updatedAt = LocalDateTime.now()
      )

    def addDebit(amount: Money): Account =
      account.accountType match
        case AccountType.Asset | AccountType.Expense =>
          account.updateBalance(account.balance + amount)
        case _ =>
          account.updateBalance(account.balance - amount)

    def addCredit(amount: Money): Account =
      account.accountType match
        case AccountType.Liability | AccountType.Equity | AccountType.Revenue =>
          account.updateBalance(account.balance + amount)
        case _ =>
          account.updateBalance(account.balance - amount)
