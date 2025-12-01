package com.example.accounting.domain.balance

import java.time.{LocalDate, LocalDateTime}

/**
 * 日次勘定科目残高エンティティ
 *
 * @param entryDate 起票日
 * @param accountCode 勘定科目コード
 * @param subAccountCode 補助科目コード
 * @param departmentCode 部門コード
 * @param projectCode プロジェクトコード
 * @param settlementFlag 決算仕訳フラグ（0=通常、1=決算）
 * @param debitAmount 借方金額
 * @param creditAmount 貸方金額
 * @param createdAt 作成日時
 * @param updatedAt 更新日時
 */
case class DailyAccountBalance(
    entryDate: LocalDate,
    accountCode: String,
    subAccountCode: String,
    departmentCode: String,
    projectCode: String,
    settlementFlag: Int,
    debitAmount: BigDecimal,
    creditAmount: BigDecimal,
    createdAt: LocalDateTime,
    updatedAt: LocalDateTime,
):
  /** 残高を計算（借方金額 - 貸方金額） */
  def balance: BigDecimal = debitAmount - creditAmount

  /** 通常仕訳かどうか */
  def isNormalEntry: Boolean = settlementFlag == 0

  /** 決算仕訳かどうか */
  def isSettlementEntry: Boolean = settlementFlag == 1

/**
 * 日次勘定科目残高の複合主キー
 */
case class DailyAccountBalanceKey(
    entryDate: LocalDate,
    accountCode: String,
    subAccountCode: String,
    departmentCode: String,
    projectCode: String,
    settlementFlag: Int,
)

/**
 * 日次残高更新レコード
 */
case class DailyBalanceUpdateRecord(
    entryDate: LocalDate,
    accountCode: String,
    subAccountCode: String,
    departmentCode: String,
    projectCode: String,
    settlementFlag: Int,
    debitAmount: BigDecimal,
    creditAmount: BigDecimal,
)
