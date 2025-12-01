package com.example.accounting.domain.balance

import java.time.LocalDateTime

/**
 * 月次勘定科目残高エンティティ
 *
 * @param fiscalYear 決算期（会計年度）
 * @param month 月度（1～12）
 * @param accountCode 勘定科目コード
 * @param subAccountCode 補助科目コード
 * @param departmentCode 部門コード
 * @param projectCode プロジェクトコード
 * @param settlementFlag 決算仕訳フラグ（0=通常、1=決算）
 * @param openingBalance 月初残高
 * @param debitAmount 借方金額
 * @param creditAmount 貸方金額
 * @param closingBalance 月末残高
 * @param createdAt 作成日時
 * @param updatedAt 更新日時
 */
case class MonthlyAccountBalance(
    fiscalYear: Int,
    month: Int,
    accountCode: String,
    subAccountCode: String,
    departmentCode: String,
    projectCode: String,
    settlementFlag: Int,
    openingBalance: BigDecimal,
    debitAmount: BigDecimal,
    creditAmount: BigDecimal,
    closingBalance: BigDecimal,
    createdAt: LocalDateTime,
    updatedAt: LocalDateTime,
):
  /** 月末残高を再計算（月初残高 + 借方金額 - 貸方金額） */
  def calculateClosingBalance: BigDecimal =
    openingBalance + debitAmount - creditAmount

  /** 残高計算が正しいかを検証 */
  def isBalanceValid: Boolean =
    closingBalance == calculateClosingBalance

  /** 通常仕訳かどうか */
  def isNormalEntry: Boolean = settlementFlag == 0

  /** 決算仕訳かどうか */
  def isSettlementEntry: Boolean = settlementFlag == 1

/**
 * 月次勘定科目残高の複合主キー
 */
case class MonthlyAccountBalanceKey(
    fiscalYear: Int,
    month: Int,
    accountCode: String,
    subAccountCode: String,
    departmentCode: String,
    projectCode: String,
    settlementFlag: Int,
)
