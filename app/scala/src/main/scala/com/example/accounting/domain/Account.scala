package com.example.accounting.domain

import java.time.LocalDateTime

/**
 * 勘定科目種別
 */
enum AccountType(val value: String):
  case 資産 extends AccountType("資産")
  case 負債 extends AccountType("負債")
  case 純資産 extends AccountType("純資産")
  case 収益 extends AccountType("収益")
  case 費用 extends AccountType("費用")

object AccountType:
  def fromString(s: String): AccountType =
    s match
      case "資産"   => 資産
      case "負債"   => 負債
      case "純資産" => 純資産
      case "収益"   => 収益
      case "費用"   => 費用
      case other    => throw new IllegalArgumentException(s"Unknown account type: $other")

/**
 * 勘定科目マスタ
 */
case class Account(
    accountId: Option[Int],
    accountCode: String,
    accountName: String,
    accountType: AccountType,
    balance: BigDecimal,
    bsplDistinction: Option[String],
    transactionElement: Option[String],
    expenseDistinction: Option[String],
    isSummaryAccount: Boolean,
    displayOrder: Option[Int],
    isAggregationTarget: Boolean,
    accountKana: Option[String],
    taxCode: Option[String],
    createdAt: LocalDateTime,
    updatedAt: LocalDateTime,
):
  /** 貸借対照表科目かどうか */
  def isBalanceSheetAccount: Boolean = bsplDistinction.contains("B")

  /** 損益計算書科目かどうか */
  def isProfitLossAccount: Boolean = bsplDistinction.contains("P")

  /** 借方科目かどうか（資産・費用） */
  def isDebitAccount: Boolean = accountType match
    case AccountType.資産 | AccountType.費用 => true
    case _                                   => false

  /** 貸方科目かどうか（負債・純資産・収益） */
  def isCreditAccount: Boolean = !isDebitAccount
