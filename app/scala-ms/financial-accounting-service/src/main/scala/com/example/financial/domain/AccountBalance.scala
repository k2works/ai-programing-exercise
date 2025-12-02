package com.example.financial.domain

import java.time.LocalDateTime
import com.example.common.domain.*

/**
 * 勘定科目残高エンティティ
 *
 * 特定の会計年度における勘定科目の残高を表現します。
 */
case class AccountBalance(
    balanceId: Option[Long],
    accountCode: AccountCode,
    fiscalYear: FiscalYear,
    debitTotal: Money,
    creditTotal: Money,
    balance: Money,
    createdAt: LocalDateTime,
    updatedAt: LocalDateTime
)

object AccountBalance:
  /**
   * 新規残高レコードを作成
   */
  def create(
      accountCode: AccountCode,
      fiscalYear: FiscalYear
  ): AccountBalance =
    val now = LocalDateTime.now()
    AccountBalance(
      balanceId = None,
      accountCode = accountCode,
      fiscalYear = fiscalYear,
      debitTotal = Money.Zero,
      creditTotal = Money.Zero,
      balance = Money.Zero,
      createdAt = now,
      updatedAt = now
    )

  extension (ab: AccountBalance)
    /**
     * 借方金額を加算
     */
    def addDebit(amount: Money): AccountBalance =
      val newDebitTotal = ab.debitTotal + amount
      ab.copy(
        debitTotal = newDebitTotal,
        balance = newDebitTotal - ab.creditTotal,
        updatedAt = LocalDateTime.now()
      )

    /**
     * 貸方金額を加算
     */
    def addCredit(amount: Money): AccountBalance =
      val newCreditTotal = ab.creditTotal + amount
      ab.copy(
        creditTotal = newCreditTotal,
        balance = ab.debitTotal - newCreditTotal,
        updatedAt = LocalDateTime.now()
      )

/**
 * 監査ログエンティティ
 */
case class AuditLog(
    logId: Option[Long],
    entityType: String,
    entityId: String,
    action: AuditAction,
    changedData: String,
    userId: String,
    createdAt: LocalDateTime
)

object AuditLog:
  def create(
      entityType: String,
      entityId: String,
      action: AuditAction,
      changedData: String,
      userId: String
  ): AuditLog =
    AuditLog(
      logId = None,
      entityType = entityType,
      entityId = entityId,
      action = action,
      changedData = changedData,
      userId = userId,
      createdAt = LocalDateTime.now()
    )
