package com.example.management.domain

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.example.common.domain.*

class FinancialRatiosSpec extends AnyFlatSpec with Matchers:

  "FinancialRatioCalculator.analyze" should "収益性指標を正しく計算する" in {
    val data = createSampleData()
    val result = FinancialRatioCalculator.analyze(data)

    // 売上総利益率 = 売上総利益 / 売上高 = 4,000,000 / 10,000,000 = 0.4
    result.profitability.grossProfitMargin shouldBe BigDecimal("0.4000")

    // 営業利益率 = 営業利益 / 売上高 = 2,000,000 / 10,000,000 = 0.2
    result.profitability.operatingProfitMargin shouldBe BigDecimal("0.2000")

    // 販管費率 = 販管費 / 売上高 = 2,000,000 / 10,000,000 = 0.2
    result.profitability.sellingExpenseRatio shouldBe BigDecimal("0.2000")
  }

  it should "効率性指標を正しく計算する" in {
    val data = createSampleData()
    val result = FinancialRatioCalculator.analyze(data)

    // 総資産回転率 = 売上高 / 総資産 = 10,000,000 / 20,000,000 = 0.5
    result.efficiency.totalAssetTurnover shouldBe BigDecimal("0.5000")

    // 固定資産回転率 = 売上高 / 固定資産 = 10,000,000 / 8,000,000 = 1.25
    result.efficiency.fixedAssetTurnover shouldBe BigDecimal("1.2500")
  }

  it should "安全性指標を正しく計算する" in {
    val data = createSampleData()
    val result = FinancialRatioCalculator.analyze(data)

    // 流動比率 = 流動資産 / 流動負債 * 100 = 12,000,000 / 6,000,000 * 100 = 200
    result.safety.currentRatio shouldBe BigDecimal("200.0000")

    // 自己資本比率 = 純資産 / 総資産 * 100 = 10,000,000 / 20,000,000 * 100 = 50
    result.safety.equityRatio shouldBe BigDecimal("50.0000")

    // 負債比率 = 負債 / 純資産 * 100 = 10,000,000 / 10,000,000 * 100 = 100
    result.safety.debtRatio shouldBe BigDecimal("100.0000")
  }

  it should "ゼロ除算を安全に処理する" in {
    val data = FinancialData.empty(FiscalYear.unsafeFrom(2024))
    val result = FinancialRatioCalculator.analyze(data)

    result.profitability.grossProfitMargin shouldBe BigDecimal(0)
    result.efficiency.totalAssetTurnover shouldBe BigDecimal(0)
    result.safety.currentRatio shouldBe BigDecimal(0)
  }

  "TrendAnalysis.analyze" should "複数期間のトレンドを分析する" in {
    val data2023 = createSampleData(2023, salesMultiplier = 0.8)
    val data2024 = createSampleData(2024, salesMultiplier = 1.0)

    val trend = TrendAnalysis.analyze(List(data2023, data2024))

    // 売上成長率 = (10,000,000 - 8,000,000) / 8,000,000 * 100 = 25%
    trend.salesGrowthRate shouldBe BigDecimal("25.00")
    trend.operatingProfitMarginTrend shouldBe "stable"
  }

  it should "単一期間の場合はデフォルト値を返す" in {
    val data = createSampleData()
    val trend = TrendAnalysis.analyze(List(data))

    trend.salesGrowthRate shouldBe BigDecimal(0)
    trend.operatingProfitMarginTrend shouldBe "stable"
  }

  private def createSampleData(
      year: Int = 2024,
      salesMultiplier: Double = 1.0
  ): FinancialData =
    val sales = BigDecimal(10000000) * salesMultiplier
    FinancialData(
      fiscalYear = FiscalYear.unsafeFrom(year),
      sales = Money(sales),
      costOfSales = Money(sales * 0.6),      // 売上原価率 60%
      grossProfit = Money(sales * 0.4),      // 売上総利益率 40%
      sellingGeneralExpenses = Money(sales * 0.2), // 販管費率 20%
      operatingProfit = Money(sales * 0.2),  // 営業利益率 20%
      nonOperatingIncome = Money(100000),
      nonOperatingExpenses = Money(50000),
      ordinaryProfit = Money(sales * 0.2 + 50000),
      currentAssets = Money(12000000),
      fixedAssets = Money(8000000),
      totalAssets = Money(20000000),
      currentLiabilities = Money(6000000),
      fixedLiabilities = Money(4000000),
      totalLiabilities = Money(10000000),
      equity = Money(10000000)
    )
