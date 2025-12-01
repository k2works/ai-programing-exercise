package com.example.accounting.domain.financial

import java.time.LocalDate

/**
 * 貸借対照表（Balance Sheet / B/S）
 *
 * @param asOfDate 基準日
 * @param assets 資産項目
 * @param liabilities 負債項目
 * @param equity 純資産項目
 * @param totalAssets 総資産
 * @param totalLiabilities 総負債
 * @param totalEquity 総純資産
 * @param totalLiabilitiesAndEquity 負債・純資産合計
 */
case class BalanceSheet(
    asOfDate: LocalDate,
    assets: List[BalanceSheetItem],
    liabilities: List[BalanceSheetItem],
    equity: List[BalanceSheetItem],
    totalAssets: BigDecimal,
    totalLiabilities: BigDecimal,
    totalEquity: BigDecimal,
    totalLiabilitiesAndEquity: BigDecimal,
):
  /**
   * 貸借平均の原則が成立しているかを検証
   * （資産 = 負債 + 純資産）
   */
  def isBalanced: Boolean =
    totalAssets == totalLiabilities + totalEquity
