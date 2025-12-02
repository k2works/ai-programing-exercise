package com.example.management.domain

import com.example.common.domain.*

/**
 * 財務比率分析結果
 *
 * 管理会計コンテキストにおける財務分析の結果を表現します。
 */
case class AnalysisResult(
    fiscalYear: FiscalYear,
    profitability: Profitability,
    efficiency: Efficiency,
    safety: Safety
)

/**
 * 収益性指標
 */
case class Profitability(
    grossProfitMargin: BigDecimal,     // 売上総利益率
    operatingProfitMargin: BigDecimal, // 営業利益率
    ordinaryProfitMargin: BigDecimal,  // 経常利益率
    sellingExpenseRatio: BigDecimal    // 販管費率
)

/**
 * 効率性指標
 */
case class Efficiency(
    totalAssetTurnover: BigDecimal,  // 総資産回転率
    fixedAssetTurnover: BigDecimal   // 固定資産回転率
)

/**
 * 安全性指標
 */
case class Safety(
    currentRatio: BigDecimal,  // 流動比率
    equityRatio: BigDecimal,   // 自己資本比率
    debtRatio: BigDecimal      // 負債比率
)

/**
 * 財務比率計算機
 */
object FinancialRatioCalculator:

  /**
   * 財務データから分析結果を計算
   */
  def analyze(data: FinancialData): AnalysisResult =
    AnalysisResult(
      fiscalYear = data.fiscalYear,
      profitability = calculateProfitability(data),
      efficiency = calculateEfficiency(data),
      safety = calculateSafety(data)
    )

  /**
   * 収益性指標を計算
   */
  private def calculateProfitability(data: FinancialData): Profitability =
    val sales = data.sales.value

    Profitability(
      grossProfitMargin = safeRatio(data.grossProfit.value, sales),
      operatingProfitMargin = safeRatio(data.operatingProfit.value, sales),
      ordinaryProfitMargin = safeRatio(data.ordinaryProfit.value, sales),
      sellingExpenseRatio = safeRatio(data.sellingGeneralExpenses.value, sales)
    )

  /**
   * 効率性指標を計算
   */
  private def calculateEfficiency(data: FinancialData): Efficiency =
    val sales = data.sales.value

    Efficiency(
      totalAssetTurnover = safeRatio(sales, data.totalAssets.value),
      fixedAssetTurnover = safeRatio(sales, data.fixedAssets.value)
    )

  /**
   * 安全性指標を計算
   */
  private def calculateSafety(data: FinancialData): Safety =
    Safety(
      currentRatio = safeRatio(data.currentAssets.value, data.currentLiabilities.value) * 100,
      equityRatio = safeRatio(data.equity.value, data.totalAssets.value) * 100,
      debtRatio = safeRatio(data.totalLiabilities.value, data.equity.value) * 100
    )

  /**
   * 安全な除算（ゼロ除算を防ぐ）
   */
  private def safeRatio(numerator: BigDecimal, denominator: BigDecimal): BigDecimal =
    if denominator == 0 then BigDecimal(0)
    else (numerator / denominator).setScale(4, BigDecimal.RoundingMode.HALF_UP)

/**
 * 複数期間の比較分析結果
 */
case class ComparisonResult(
    results: List[AnalysisResult],
    trends: TrendAnalysis
)

/**
 * トレンド分析
 */
case class TrendAnalysis(
    salesGrowthRate: BigDecimal,           // 売上成長率
    profitGrowthRate: BigDecimal,          // 利益成長率
    operatingProfitMarginTrend: String,    // 営業利益率推移（"up", "down", "stable"）
    equityRatioTrend: String               // 自己資本比率推移
)

object TrendAnalysis:
  /**
   * 複数期間のデータからトレンドを分析
   */
  def analyze(dataList: List[FinancialData]): TrendAnalysis =
    if dataList.size < 2 then
      TrendAnalysis(
        salesGrowthRate = BigDecimal(0),
        profitGrowthRate = BigDecimal(0),
        operatingProfitMarginTrend = "stable",
        equityRatioTrend = "stable"
      )
    else
      val sorted = dataList.sortBy(_.fiscalYear.value)
      val first = sorted.head
      val last = sorted.last

      val salesGrowth = calculateGrowthRate(first.sales.value, last.sales.value)
      val profitGrowth = calculateGrowthRate(first.operatingProfit.value, last.operatingProfit.value)

      val firstMargin = FinancialRatioCalculator.analyze(first).profitability.operatingProfitMargin
      val lastMargin = FinancialRatioCalculator.analyze(last).profitability.operatingProfitMargin

      val firstEquityRatio = FinancialRatioCalculator.analyze(first).safety.equityRatio
      val lastEquityRatio = FinancialRatioCalculator.analyze(last).safety.equityRatio

      TrendAnalysis(
        salesGrowthRate = salesGrowth,
        profitGrowthRate = profitGrowth,
        operatingProfitMarginTrend = determineTrend(firstMargin, lastMargin),
        equityRatioTrend = determineTrend(firstEquityRatio, lastEquityRatio)
      )

  private def calculateGrowthRate(base: BigDecimal, current: BigDecimal): BigDecimal =
    if base == 0 then BigDecimal(0)
    else ((current - base) / base * 100).setScale(2, BigDecimal.RoundingMode.HALF_UP)

  private def determineTrend(first: BigDecimal, last: BigDecimal): String =
    val diff = last - first
    if diff > 0.01 then "up"
    else if diff < -0.01 then "down"
    else "stable"
