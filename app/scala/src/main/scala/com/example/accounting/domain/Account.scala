package com.example.accounting.domain

import scalikejdbc.*
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
  def fromString(s: String): AccountType = s match
    case "資産"   => 資産
    case "負債"   => 負債
    case "純資産" => 純資産
    case "収益"   => 収益
    case "費用"   => 費用
    case other    => throw IllegalArgumentException(s"Unknown account type: $other")

/**
 * 勘定科目マスタのケースクラス
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
  /**
   * 貸借対照表科目かどうか
   */
  def isBalanceSheetAccount: Boolean = bsplDistinction.contains("B")

  /**
   * 損益計算書科目かどうか
   */
  def isProfitLossAccount: Boolean = bsplDistinction.contains("P")

  /**
   * 借方科目かどうか（資産・費用）
   */
  def isDebitAccount: Boolean = accountType match
    case AccountType.資産 | AccountType.費用 => true
    case _                                   => false

  /**
   * 貸方科目かどうか（負債・純資産・収益）
   */
  def isCreditAccount: Boolean = !isDebitAccount

object Account extends SQLSyntaxSupport[Account]:
  override val tableName = "勘定科目マスタ"
  override val columns = Seq(
    "勘定科目ID",
    "勘定科目コード",
    "勘定科目名",
    "勘定科目種別",
    "残高",
    "BSPL区分",
    "取引要素区分",
    "費用区分",
    "合計科目",
    "表示順序",
    "集計対象",
    "勘定科目カナ",
    "課税取引コード",
    "作成日時",
    "更新日時",
  )

  def apply(rs: WrappedResultSet): Account = new Account(
    accountId = Some(rs.int("勘定科目ID")),
    accountCode = rs.string("勘定科目コード"),
    accountName = rs.string("勘定科目名"),
    accountType = AccountType.fromString(rs.string("勘定科目種別")),
    balance = rs.bigDecimal("残高"),
    bsplDistinction = rs.stringOpt("BSPL区分"),
    transactionElement = rs.stringOpt("取引要素区分"),
    expenseDistinction = rs.stringOpt("費用区分"),
    isSummaryAccount = rs.boolean("合計科目"),
    displayOrder = rs.intOpt("表示順序"),
    isAggregationTarget = rs.boolean("集計対象"),
    accountKana = rs.stringOpt("勘定科目カナ"),
    taxCode = rs.stringOpt("課税取引コード"),
    createdAt = rs.localDateTime("作成日時"),
    updatedAt = rs.localDateTime("更新日時"),
  )
