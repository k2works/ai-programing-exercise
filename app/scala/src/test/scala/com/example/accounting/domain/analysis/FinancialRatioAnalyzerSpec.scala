package com.example.accounting.domain.analysis

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * FinancialRatioAnalyzer のユニットテスト
 */
class FinancialRatioAnalyzerSpec extends AnyFlatSpec with Matchers:

  // D社 令和3年度のテストデータ
  private val fy2021Data = FinancialData.fy2021Data

  // D社 令和4年度のテストデータ
  private val fy2022Data = FinancialData.fy2022Data

  behavior of "FinancialRatioAnalyzer.analyze"

  it should "売上高総利益率を正しく計算できる" in {
    val result = FinancialRatioAnalyzer.analyze(fy2021Data)

    result.isRight shouldBe true
    result.foreach { r =>
      // 売上高総利益率 = 売上総利益 / 売上高 × 100
      // = 3,610,249,000 / 5,796,105,000 × 100 ≈ 62.29%
      r.profitability.grossProfitMargin shouldBe BigDecimal("62.29") +- BigDecimal("0.01")
    }
  }

  it should "売上高営業利益率を正しく計算できる" in {
    val result = FinancialRatioAnalyzer.analyze(fy2021Data)

    result.isRight shouldBe true
    result.foreach { r =>
      // 売上高営業利益率 = 営業利益 / 売上高 × 100
      // = 985,027,000 / 5,796,105,000 × 100 ≈ 17.00%
      r.profitability.operatingProfitMargin shouldBe BigDecimal("17.00") +- BigDecimal("0.01")
    }
  }

  it should "売上高経常利益率を正しく計算できる" in {
    val result = FinancialRatioAnalyzer.analyze(fy2021Data)

    result.isRight shouldBe true
    result.foreach { r =>
      // 売上高経常利益率 = 経常利益 / 売上高 × 100
      // = 982,719,000 / 5,796,105,000 × 100 ≈ 16.96%
      r.profitability.ordinaryProfitMargin shouldBe BigDecimal("16.96") +- BigDecimal("0.01")
    }
  }

  it should "売上高販管費比率を正しく計算できる" in {
    val result = FinancialRatioAnalyzer.analyze(fy2021Data)

    result.isRight shouldBe true
    result.foreach { r =>
      // 売上高販管費比率 = 販管費 / 売上高 × 100
      // = 2,625,222,000 / 5,796,105,000 × 100 ≈ 45.29%
      r.profitability.sellingExpenseRatio shouldBe BigDecimal("45.29") +- BigDecimal("0.01")
    }
  }

  it should "総資本回転率を正しく計算できる" in {
    val result = FinancialRatioAnalyzer.analyze(fy2021Data)

    result.isRight shouldBe true
    result.foreach { r =>
      // 総資本回転率 = 売上高 / 総資産
      // = 5,796,105,000 / 2,863,166,000 ≈ 2.02回
      r.efficiency.totalAssetTurnover shouldBe BigDecimal("2.02") +- BigDecimal("0.01")
    }
  }

  it should "有形固定資産回転率を正しく計算できる" in {
    val result = FinancialRatioAnalyzer.analyze(fy2021Data)

    result.isRight shouldBe true
    result.foreach { r =>
      // 有形固定資産回転率 = 売上高 / 固定資産
      // = 5,796,105,000 / 186,973,000 ≈ 30.99回
      r.efficiency.fixedAssetTurnover shouldBe BigDecimal("30.99") +- BigDecimal("0.01")
    }
  }

  it should "流動比率を正しく計算できる" in {
    val result = FinancialRatioAnalyzer.analyze(fy2021Data)

    result.isRight shouldBe true
    result.foreach { r =>
      // 流動比率 = 流動資産 / 流動負債 × 100
      // = 2,676,193,000 / 851,394,000 × 100 ≈ 314.33%
      r.safety.currentRatio shouldBe BigDecimal("314.33") +- BigDecimal("0.01")
    }
  }

  it should "自己資本比率を正しく計算できる" in {
    val result = FinancialRatioAnalyzer.analyze(fy2021Data)

    result.isRight shouldBe true
    result.foreach { r =>
      // 自己資本比率 = 純資産 / 総資産 × 100
      // = 1,989,272,000 / 2,863,166,000 × 100 ≈ 69.48%
      r.safety.equityRatio shouldBe BigDecimal("69.48") +- BigDecimal("0.01")
    }
  }

  it should "負債比率を正しく計算できる" in {
    val result = FinancialRatioAnalyzer.analyze(fy2021Data)

    result.isRight shouldBe true
    result.foreach { r =>
      // 負債比率 = 負債合計 / 純資産 × 100
      // = 873,894,000 / 1,989,272,000 × 100 ≈ 43.93%
      r.safety.debtRatio shouldBe BigDecimal("43.93") +- BigDecimal("0.01")
    }
  }

  it should "令和4年度のデータでも正しく計算できる" in {
    val result = FinancialRatioAnalyzer.analyze(fy2022Data)

    result.isRight shouldBe true
    result.foreach { r =>
      r.fiscalYear shouldBe 2022
      // 売上高営業利益率 = 527,037,000 / 4,547,908,000 × 100 ≈ 11.59%
      r.profitability.operatingProfitMargin shouldBe BigDecimal("11.59") +- BigDecimal("0.01")
    }
  }

  behavior of "FinancialRatioAnalyzer.compare"

  it should "複数年度の比較分析ができる" in {
    val result = FinancialRatioAnalyzer.compare(List(fy2021Data, fy2022Data))

    result.isRight shouldBe true
    result.foreach { r =>
      r.results should have size 2
      r.results(0).fiscalYear shouldBe 2021
      r.results(1).fiscalYear shouldBe 2022
    }
  }

  it should "2年度間の変化率を計算できる" in {
    val result = FinancialRatioAnalyzer.compare(List(fy2021Data, fy2022Data))

    result.isRight shouldBe true
    result.foreach { r =>
      r.changes shouldBe defined
      r.changes.foreach { changes =>
        // 営業利益率の変化 = 11.59 - 17.00 ≈ -5.41
        changes.profitability.operatingProfitMarginChange shouldBe BigDecimal("-5.41") +- BigDecimal("0.1")
      }
    }
  }

  it should "単年度のデータでは変化率がない" in {
    val result = FinancialRatioAnalyzer.compare(List(fy2021Data))

    result.isRight shouldBe true
    result.foreach { r =>
      r.results should have size 1
      r.changes shouldBe None
    }
  }

  behavior of "エラーハンドリング"

  it should "売上高がゼロの場合はエラーを返す" in {
    val invalidData = fy2021Data.copy(sales = BigDecimal(0))
    val result = FinancialRatioAnalyzer.analyze(invalidData)

    result.isLeft shouldBe true
    result.left.foreach { error =>
      error shouldBe a[AnalysisError.InvalidDenominator]
    }
  }

  it should "流動負債がゼロの場合はエラーを返す" in {
    val invalidData = fy2021Data.copy(currentLiabilities = BigDecimal(0))
    val result = FinancialRatioAnalyzer.analyze(invalidData)

    result.isLeft shouldBe true
    result.left.foreach { error =>
      error shouldBe a[AnalysisError.InvalidDenominator]
    }
  }

  it should "固定資産がゼロの場合はエラーを返す" in {
    val invalidData = fy2021Data.copy(fixedAssets = BigDecimal(0))
    val result = FinancialRatioAnalyzer.analyze(invalidData)

    result.isLeft shouldBe true
    result.left.foreach { error =>
      error shouldBe a[AnalysisError.InvalidAsset]
    }
  }
