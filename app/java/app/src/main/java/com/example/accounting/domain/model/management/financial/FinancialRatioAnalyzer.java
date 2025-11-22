package com.example.accounting.domain.model.management.financial;

import java.math.BigDecimal;
import java.math.RoundingMode;

/**
 * 財務分析指標を計算するクラス
 *
 * 収益性、効率性、安全性の各指標を計算します。
 */
public class FinancialRatioAnalyzer {

    private static final int SCALE = 2;  // 小数点以下の桁数
    private static final RoundingMode ROUNDING_MODE = RoundingMode.HALF_UP;  // 四捨五入

    /**
     * 財務データを分析して各種指標を計算
     *
     * @param data 財務データ
     * @return 分析結果
     */
    public AnalysisResult analyze(FinancialData data) {
        return new AnalysisResult(
            calculateProfitability(data),
            calculateEfficiency(data),
            calculateSafety(data)
        );
    }

    /**
     * 収益性指標を計算
     */
    private Profitability calculateProfitability(FinancialData data) {
        BigDecimal operatingProfitMargin = calculateOperatingProfitMargin(
            data.sales(),
            data.operatingProfit()
        );
        return new Profitability(operatingProfitMargin);
    }

    /**
     * 効率性指標を計算
     */
    private Efficiency calculateEfficiency(FinancialData data) {
        BigDecimal totalAssetTurnover = calculateTotalAssetTurnover(
            data.sales(),
            data.totalAssets()
        );
        BigDecimal tangibleFixedAssetTurnover = calculateTangibleFixedAssetTurnover(
            data.sales(),
            data.tangibleFixedAssets()
        );
        return new Efficiency(totalAssetTurnover, tangibleFixedAssetTurnover);
    }

    /**
     * 安全性指標を計算
     */
    private Safety calculateSafety(FinancialData data) {
        BigDecimal currentRatio = calculateCurrentRatio(
            data.currentAssets(),
            data.currentLiabilities()
        );
        BigDecimal quickRatio = calculateQuickRatio(
            data.quickAssets(),
            data.currentLiabilities()
        );
        BigDecimal equityRatio = calculateEquityRatio(
            data.equity(),
            data.totalAssets()
        );
        return new Safety(currentRatio, quickRatio, equityRatio);
    }

    // 収益性指標

    /**
     * 売上高営業利益率を計算
     *
     * @param sales 売上高
     * @param operatingProfit 営業利益
     * @return 売上高営業利益率（%）
     */
    private BigDecimal calculateOperatingProfitMargin(
        BigDecimal sales,
        BigDecimal operatingProfit
    ) {
        if (sales.compareTo(BigDecimal.ZERO) == 0) {
            throw new IllegalArgumentException("売上高がゼロのため計算できません");
        }
        return operatingProfit
            .divide(sales, 4, ROUNDING_MODE)
            .multiply(new BigDecimal("100"))
            .setScale(SCALE, ROUNDING_MODE);
    }

    // 効率性指標

    /**
     * 総資本回転率を計算
     *
     * @param sales 売上高
     * @param totalAssets 総資産
     * @return 総資本回転率（回）
     */
    private BigDecimal calculateTotalAssetTurnover(
        BigDecimal sales,
        BigDecimal totalAssets
    ) {
        if (totalAssets.compareTo(BigDecimal.ZERO) == 0) {
            throw new IllegalArgumentException("総資本がゼロのため計算できません");
        }
        return sales
            .divide(totalAssets, SCALE, ROUNDING_MODE);
    }

    /**
     * 有形固定資産回転率を計算
     *
     * @param sales 売上高
     * @param tangibleFixedAssets 有形固定資産
     * @return 有形固定資産回転率（回）
     */
    private BigDecimal calculateTangibleFixedAssetTurnover(
        BigDecimal sales,
        BigDecimal tangibleFixedAssets
    ) {
        if (tangibleFixedAssets.compareTo(BigDecimal.ZERO) == 0) {
            throw new IllegalArgumentException("有形固定資産がゼロのため計算できません");
        }
        return sales
            .divide(tangibleFixedAssets, SCALE, ROUNDING_MODE);
    }

    // 安全性指標

    /**
     * 流動比率を計算
     *
     * @param currentAssets 流動資産
     * @param currentLiabilities 流動負債
     * @return 流動比率（%）
     */
    private BigDecimal calculateCurrentRatio(
        BigDecimal currentAssets,
        BigDecimal currentLiabilities
    ) {
        if (currentLiabilities.compareTo(BigDecimal.ZERO) == 0) {
            throw new IllegalArgumentException("流動負債がゼロのため計算できません");
        }
        return currentAssets
            .divide(currentLiabilities, 4, ROUNDING_MODE)
            .multiply(new BigDecimal("100"))
            .setScale(SCALE, ROUNDING_MODE);
    }

    /**
     * 当座比率を計算
     *
     * @param quickAssets 当座資産
     * @param currentLiabilities 流動負債
     * @return 当座比率（%）
     */
    private BigDecimal calculateQuickRatio(
        BigDecimal quickAssets,
        BigDecimal currentLiabilities
    ) {
        if (currentLiabilities.compareTo(BigDecimal.ZERO) == 0) {
            throw new IllegalArgumentException("流動負債がゼロのため計算できません");
        }
        return quickAssets
            .divide(currentLiabilities, 4, ROUNDING_MODE)
            .multiply(new BigDecimal("100"))
            .setScale(SCALE, ROUNDING_MODE);
    }

    /**
     * 自己資本比率を計算
     *
     * @param equity 純資産
     * @param totalAssets 総資産
     * @return 自己資本比率（%）
     */
    private BigDecimal calculateEquityRatio(
        BigDecimal equity,
        BigDecimal totalAssets
    ) {
        if (totalAssets.compareTo(BigDecimal.ZERO) == 0) {
            throw new IllegalArgumentException("総資本がゼロのため計算できません");
        }
        return equity
            .divide(totalAssets, 4, ROUNDING_MODE)
            .multiply(new BigDecimal("100"))
            .setScale(SCALE, ROUNDING_MODE);
    }
}
