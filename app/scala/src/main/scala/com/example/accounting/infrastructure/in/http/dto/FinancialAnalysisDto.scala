package com.example.accounting.infrastructure.in.http.dto

import com.example.accounting.domain.analysis.*

/**
 * 収益性指標レスポンス
 */
case class ProfitabilityResponse(
  grossProfitMargin: BigDecimal,
  operatingProfitMargin: BigDecimal,
  ordinaryProfitMargin: BigDecimal,
  sellingExpenseRatio: BigDecimal
)

object ProfitabilityResponse:
  def fromDomain(p: Profitability): ProfitabilityResponse =
    ProfitabilityResponse(
      grossProfitMargin = p.grossProfitMargin,
      operatingProfitMargin = p.operatingProfitMargin,
      ordinaryProfitMargin = p.ordinaryProfitMargin,
      sellingExpenseRatio = p.sellingExpenseRatio
    )

/**
 * 効率性指標レスポンス
 */
case class EfficiencyResponse(
  totalAssetTurnover: BigDecimal,
  fixedAssetTurnover: BigDecimal
)

object EfficiencyResponse:
  def fromDomain(e: Efficiency): EfficiencyResponse =
    EfficiencyResponse(
      totalAssetTurnover = e.totalAssetTurnover,
      fixedAssetTurnover = e.fixedAssetTurnover
    )

/**
 * 安全性指標レスポンス
 */
case class SafetyResponse(
  currentRatio: BigDecimal,
  equityRatio: BigDecimal,
  debtRatio: BigDecimal
)

object SafetyResponse:
  def fromDomain(s: Safety): SafetyResponse =
    SafetyResponse(
      currentRatio = s.currentRatio,
      equityRatio = s.equityRatio,
      debtRatio = s.debtRatio
    )

/**
 * 財務分析結果レスポンス
 */
case class FinancialAnalysisResponse(
  fiscalYear: Int,
  profitability: ProfitabilityResponse,
  efficiency: EfficiencyResponse,
  safety: SafetyResponse
)

object FinancialAnalysisResponse:
  def fromDomain(r: AnalysisResult): FinancialAnalysisResponse =
    FinancialAnalysisResponse(
      fiscalYear = r.fiscalYear,
      profitability = ProfitabilityResponse.fromDomain(r.profitability),
      efficiency = EfficiencyResponse.fromDomain(r.efficiency),
      safety = SafetyResponse.fromDomain(r.safety)
    )

/**
 * 収益性指標の変化レスポンス
 */
case class ProfitabilityChangeResponse(
  grossProfitMarginChange: BigDecimal,
  operatingProfitMarginChange: BigDecimal,
  ordinaryProfitMarginChange: BigDecimal,
  sellingExpenseRatioChange: BigDecimal
)

object ProfitabilityChangeResponse:
  def fromDomain(c: ProfitabilityChange): ProfitabilityChangeResponse =
    ProfitabilityChangeResponse(
      grossProfitMarginChange = c.grossProfitMarginChange,
      operatingProfitMarginChange = c.operatingProfitMarginChange,
      ordinaryProfitMarginChange = c.ordinaryProfitMarginChange,
      sellingExpenseRatioChange = c.sellingExpenseRatioChange
    )

/**
 * 効率性指標の変化レスポンス
 */
case class EfficiencyChangeResponse(
  totalAssetTurnoverChange: BigDecimal,
  fixedAssetTurnoverChange: BigDecimal
)

object EfficiencyChangeResponse:
  def fromDomain(c: EfficiencyChange): EfficiencyChangeResponse =
    EfficiencyChangeResponse(
      totalAssetTurnoverChange = c.totalAssetTurnoverChange,
      fixedAssetTurnoverChange = c.fixedAssetTurnoverChange
    )

/**
 * 安全性指標の変化レスポンス
 */
case class SafetyChangeResponse(
  currentRatioChange: BigDecimal,
  equityRatioChange: BigDecimal,
  debtRatioChange: BigDecimal
)

object SafetyChangeResponse:
  def fromDomain(c: SafetyChange): SafetyChangeResponse =
    SafetyChangeResponse(
      currentRatioChange = c.currentRatioChange,
      equityRatioChange = c.equityRatioChange,
      debtRatioChange = c.debtRatioChange
    )

/**
 * 指標変化率レスポンス
 */
case class RatioChangesResponse(
  profitability: ProfitabilityChangeResponse,
  efficiency: EfficiencyChangeResponse,
  safety: SafetyChangeResponse
)

object RatioChangesResponse:
  def fromDomain(c: RatioChanges): RatioChangesResponse =
    RatioChangesResponse(
      profitability = ProfitabilityChangeResponse.fromDomain(c.profitability),
      efficiency = EfficiencyChangeResponse.fromDomain(c.efficiency),
      safety = SafetyChangeResponse.fromDomain(c.safety)
    )

/**
 * 複数年度比較レスポンス
 */
case class FinancialComparisonResponse(
  results: List[FinancialAnalysisResponse],
  changes: Option[RatioChangesResponse]
)

object FinancialComparisonResponse:
  def fromDomain(r: ComparisonResult): FinancialComparisonResponse =
    FinancialComparisonResponse(
      results = r.results.map(FinancialAnalysisResponse.fromDomain),
      changes = r.changes.map(RatioChangesResponse.fromDomain)
    )
