package com.example.accounting.application.port.in;

import java.math.BigDecimal;
import java.util.List;

/**
 * 財務分析のユースケースを定義するインターフェース（Input Port）
 */
public interface FinancialAnalysisUseCase {

    /**
     * 指定された会計年度の財務分析を実行
     *
     * @param fiscalYear 会計年度
     * @return 財務分析結果
     */
    FinancialAnalysisResult analyzeByFiscalYear(int fiscalYear);

    /**
     * 複数期間の財務分析を比較
     *
     * @param fiscalYears 会計年度のリスト
     * @return 比較分析結果
     */
    ComparativeAnalysisResult compareMultiplePeriods(List<Integer> fiscalYears);
}

/**
 * 財務分析結果
 */
record FinancialAnalysisResult(
    int fiscalYear,
    FinancialDataDto financialData,
    RatiosDto ratios
) {}

/**
 * 財務データ DTO
 */
record FinancialDataDto(
    BigDecimal sales,
    BigDecimal operatingProfit,
    BigDecimal totalAssets,
    BigDecimal tangibleFixedAssets,
    BigDecimal currentAssets,
    BigDecimal currentLiabilities,
    BigDecimal quickAssets,
    BigDecimal equity
) {}

/**
 * 財務指標 DTO
 */
record RatiosDto(
    ProfitabilityDto profitability,
    EfficiencyDto efficiency,
    SafetyDto safety
) {}

/**
 * 収益性指標 DTO
 */
record ProfitabilityDto(
    BigDecimal operatingProfitMargin
) {}

/**
 * 効率性指標 DTO
 */
record EfficiencyDto(
    BigDecimal totalAssetTurnover,
    BigDecimal tangibleFixedAssetTurnover
) {}

/**
 * 安全性指標 DTO
 */
record SafetyDto(
    BigDecimal currentRatio,
    BigDecimal quickRatio,
    BigDecimal equityRatio
) {}

/**
 * 比較分析結果
 */
record ComparativeAnalysisResult(
    List<FinancialAnalysisResult> periods,
    TrendsDto trends
) {}

/**
 * トレンド DTO
 */
record TrendsDto(
    BigDecimal operatingProfitMarginChange,
    BigDecimal totalAssetTurnoverChange,
    BigDecimal equityRatioChange
) {}
