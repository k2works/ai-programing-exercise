package com.example.management.domain

import com.example.common.domain.*

/**
 * 財務データ
 *
 * 管理会計コンテキストにおける財務データを表現します。
 * 財務会計コンテキストから変換されたデータです。
 */
case class FinancialData(
    fiscalYear: FiscalYear,
    sales: Money,              // 売上高
    costOfSales: Money,        // 売上原価
    grossProfit: Money,        // 売上総利益
    sellingGeneralExpenses: Money, // 販管費
    operatingProfit: Money,    // 営業利益
    nonOperatingIncome: Money, // 営業外収益
    nonOperatingExpenses: Money, // 営業外費用
    ordinaryProfit: Money,     // 経常利益
    currentAssets: Money,      // 流動資産
    fixedAssets: Money,        // 固定資産
    totalAssets: Money,        // 総資産
    currentLiabilities: Money, // 流動負債
    fixedLiabilities: Money,   // 固定負債
    totalLiabilities: Money,   // 総負債
    equity: Money              // 純資産
)

object FinancialData:
  /**
   * 空の財務データを作成（テスト用）
   */
  def empty(fiscalYear: FiscalYear): FinancialData =
    FinancialData(
      fiscalYear = fiscalYear,
      sales = Money.Zero,
      costOfSales = Money.Zero,
      grossProfit = Money.Zero,
      sellingGeneralExpenses = Money.Zero,
      operatingProfit = Money.Zero,
      nonOperatingIncome = Money.Zero,
      nonOperatingExpenses = Money.Zero,
      ordinaryProfit = Money.Zero,
      currentAssets = Money.Zero,
      fixedAssets = Money.Zero,
      totalAssets = Money.Zero,
      currentLiabilities = Money.Zero,
      fixedLiabilities = Money.Zero,
      totalLiabilities = Money.Zero,
      equity = Money.Zero
    )
