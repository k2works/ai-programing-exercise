package com.example.accounting.domain.model.management.financial;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.assertj.core.api.Assertions.within;

/**
 * FinancialRatioAnalyzer: 財務指標計算クラスのテスト
 */
@DisplayName("FinancialRatioAnalyzer - 財務指標計算クラス")
class FinancialRatioAnalyzerTest {

    private FinancialRatioAnalyzer analyzer;
    private FinancialData fy2021Data;

    @BeforeEach
    void setUp() {
        analyzer = new FinancialRatioAnalyzer();

        // 令和3年度の財務データ
        fy2021Data = new FinancialData(
            2021,
            new BigDecimal("5796105000"),   // 売上高
            new BigDecimal("985027000"),    // 営業利益
            new BigDecimal("2863166000"),   // 総資産
            new BigDecimal("64500000"),     // 有形固定資産
            new BigDecimal("2676193000"),   // 流動資産
            new BigDecimal("851394000"),    // 流動負債
            new BigDecimal("2176193000"),   // 当座資産
            new BigDecimal("1989272000")    // 純資産
        );
    }

    @Test
    @DisplayName("売上高営業利益率を正しく計算できること")
    void shouldCalculateOperatingProfitMargin() {
        // When
        AnalysisResult result = analyzer.analyze(fy2021Data);

        // Then: 16.99% (985027 / 5796105 * 100)
        assertThat(result.profitability().operatingProfitMargin())
            .isCloseTo(new BigDecimal("16.99"), within(new BigDecimal("0.01")));
    }

    @Test
    @DisplayName("総資本回転率を正しく計算できること")
    void shouldCalculateTotalAssetTurnover() {
        // When
        AnalysisResult result = analyzer.analyze(fy2021Data);

        // Then: 2.02回 (5796105 / 2863166)
        assertThat(result.efficiency().totalAssetTurnover())
            .isCloseTo(new BigDecimal("2.02"), within(new BigDecimal("0.01")));
    }

    @Test
    @DisplayName("有形固定資産回転率を正しく計算できること")
    void shouldCalculateTangibleFixedAssetTurnover() {
        // When
        AnalysisResult result = analyzer.analyze(fy2021Data);

        // Then: 89.86回 (5796105 / 64500)
        assertThat(result.efficiency().tangibleFixedAssetTurnover())
            .isCloseTo(new BigDecimal("89.86"), within(new BigDecimal("0.01")));
    }

    @Test
    @DisplayName("流動比率を正しく計算できること")
    void shouldCalculateCurrentRatio() {
        // When
        AnalysisResult result = analyzer.analyze(fy2021Data);

        // Then: 314.33% (2676193 / 851394 * 100)
        assertThat(result.safety().currentRatio())
            .isCloseTo(new BigDecimal("314.33"), within(new BigDecimal("0.01")));
    }

    @Test
    @DisplayName("当座比率を正しく計算できること")
    void shouldCalculateQuickRatio() {
        // When
        AnalysisResult result = analyzer.analyze(fy2021Data);

        // Then: 255.60% (2176193 / 851394 * 100)
        assertThat(result.safety().quickRatio())
            .isCloseTo(new BigDecimal("255.60"), within(new BigDecimal("0.01")));
    }

    @Test
    @DisplayName("自己資本比率を正しく計算できること")
    void shouldCalculateEquityRatio() {
        // When
        AnalysisResult result = analyzer.analyze(fy2021Data);

        // Then: 69.48% (1989272 / 2863166 * 100)
        assertThat(result.safety().equityRatio())
            .isCloseTo(new BigDecimal("69.48"), within(new BigDecimal("0.01")));
    }

    @Test
    @DisplayName("売上高がゼロの場合は例外をスローすること")
    void shouldThrowExceptionWhenSalesIsZero() {
        // Given: 売上高がゼロの財務データ
        FinancialData invalidData = new FinancialData(
            2021,
            BigDecimal.ZERO,  // 売上高がゼロ
            new BigDecimal("100000"),
            new BigDecimal("1000000"),
            new BigDecimal("50000"),
            new BigDecimal("500000"),
            new BigDecimal("200000"),
            new BigDecimal("400000"),
            new BigDecimal("800000")
        );

        // When & Then
        assertThatThrownBy(() -> analyzer.analyze(invalidData))
            .isInstanceOf(IllegalArgumentException.class)
            .hasMessageContaining("売上高がゼロのため計算できません");
    }

    @Test
    @DisplayName("総資産がゼロの場合は例外をスローすること")
    void shouldThrowExceptionWhenTotalAssetsIsZero() {
        // Given: 総資産がゼロの財務データ
        FinancialData invalidData = new FinancialData(
            2021,
            new BigDecimal("1000000"),
            new BigDecimal("100000"),
            BigDecimal.ZERO,  // 総資産がゼロ
            new BigDecimal("50000"),
            new BigDecimal("500000"),
            new BigDecimal("200000"),
            new BigDecimal("400000"),
            BigDecimal.ZERO
        );

        // When & Then
        assertThatThrownBy(() -> analyzer.analyze(invalidData))
            .isInstanceOf(IllegalArgumentException.class)
            .hasMessageContaining("総資本がゼロのため計算できません");
    }
}
