package com.example.accounting.domain.model.management;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * FinancialData: 財務データ値オブジェクトのテスト
 */
@DisplayName("FinancialData - 財務データ値オブジェクト")
class FinancialDataTest {

    @Test
    @DisplayName("仕訳データから財務データを生成できること")
    void shouldCreateFromJournalEntries() {
        // Given: 令和3年度の仕訳データ
        var entries = List.of(
            new FinancialData.JournalEntryData("11", new BigDecimal("2676193000"), BigDecimal.ZERO),  // 流動資産
            new FinancialData.JournalEntryData("12", new BigDecimal("186973000"), BigDecimal.ZERO),   // 固定資産
            new FinancialData.JournalEntryData("21", BigDecimal.ZERO, new BigDecimal("851394000")),   // 流動負債
            new FinancialData.JournalEntryData("31", BigDecimal.ZERO, new BigDecimal("100000000")),   // 資本金
            new FinancialData.JournalEntryData("33", BigDecimal.ZERO, new BigDecimal("1889272000")),  // 利益剰余金
            new FinancialData.JournalEntryData("41", BigDecimal.ZERO, new BigDecimal("5796105000")),  // 売上高
            new FinancialData.JournalEntryData("51", new BigDecimal("2185856000"), BigDecimal.ZERO),  // 売上原価
            new FinancialData.JournalEntryData("52", new BigDecimal("2625222000"), BigDecimal.ZERO),  // 販管費
            new FinancialData.JournalEntryData("114", new BigDecimal("500000000"), BigDecimal.ZERO),  // 棚卸資産
            new FinancialData.JournalEntryData("121", new BigDecimal("64000000"), BigDecimal.ZERO)    // 有形固定資産
        );

        // When: 財務データを生成
        FinancialData financialData = FinancialData.fromJournalEntries(2021, entries);

        // Then: 各財務数値が正しく計算されていること
        assertThat(financialData.fiscalYear()).isEqualTo(2021);
        assertThat(financialData.sales()).isEqualByComparingTo(new BigDecimal("5796105000"));  // 売上高
        assertThat(financialData.operatingProfit()).isEqualByComparingTo(new BigDecimal("985027000"));  // 営業利益 = 5796105 - 2185856 - 2625222
        assertThat(financialData.totalAssets()).isEqualByComparingTo(new BigDecimal("2863166000"));  // 総資産 = 2676193 + 186973
        assertThat(financialData.tangibleFixedAssets()).isEqualByComparingTo(new BigDecimal("64000000"));  // 有形固定資産
        assertThat(financialData.currentAssets()).isEqualByComparingTo(new BigDecimal("2676193000"));  // 流動資産
        assertThat(financialData.currentLiabilities()).isEqualByComparingTo(new BigDecimal("851394000"));  // 流動負債
        assertThat(financialData.quickAssets()).isEqualByComparingTo(new BigDecimal("2176193000"));  // 当座資産 = 2676193 - 500000
        assertThat(financialData.equity()).isEqualByComparingTo(new BigDecimal("1989272000"));  // 純資産 = 100000 + 1889272
    }

    @Test
    @DisplayName("財務データの不変性が保証されること")
    void shouldBeImmutable() {
        // Given & When: 財務データを作成
        FinancialData data = new FinancialData(
            2021,
            new BigDecimal("5796105000"),
            new BigDecimal("985027000"),
            new BigDecimal("2863166000"),
            new BigDecimal("64000000"),
            new BigDecimal("2676193000"),
            new BigDecimal("851394000"),
            new BigDecimal("2176193000"),
            new BigDecimal("1989272000")
        );

        // Then: すべてのフィールドが final であり、値が保持されること
        assertThat(data.fiscalYear()).isEqualTo(2021);
        assertThat(data.sales()).isNotNull();
        // Record クラスなので、不変性は言語仕様で保証される
    }

    @Test
    @DisplayName("D社の令和3年度データを正しく生成できること")
    void shouldCreateFY2021DataForCompanyD() {
        // Given: D社令和3年度の実際のデータ
        var entries = List.of(
            new FinancialData.JournalEntryData("11", new BigDecimal("2676193000"), BigDecimal.ZERO),
            new FinancialData.JournalEntryData("12", new BigDecimal("186973000"), BigDecimal.ZERO),
            new FinancialData.JournalEntryData("21", BigDecimal.ZERO, new BigDecimal("851394000")),
            new FinancialData.JournalEntryData("31", BigDecimal.ZERO, new BigDecimal("100000000")),
            new FinancialData.JournalEntryData("33", BigDecimal.ZERO, new BigDecimal("1889272000")),
            new FinancialData.JournalEntryData("41", BigDecimal.ZERO, new BigDecimal("5796105000")),
            new FinancialData.JournalEntryData("51", new BigDecimal("2185856000"), BigDecimal.ZERO),
            new FinancialData.JournalEntryData("52", new BigDecimal("2625222000"), BigDecimal.ZERO),
            new FinancialData.JournalEntryData("114", new BigDecimal("500000000"), BigDecimal.ZERO),
            new FinancialData.JournalEntryData("121", new BigDecimal("64000000"), BigDecimal.ZERO)
        );

        // When
        FinancialData data = FinancialData.fromJournalEntries(2021, entries);

        // Then: 売上高営業利益率 = 985,027,000 / 5,796,105,000 = 約16.99%
        assertThat(data.sales()).isEqualByComparingTo(new BigDecimal("5796105000"));
        assertThat(data.operatingProfit()).isEqualByComparingTo(new BigDecimal("985027000"));
    }
}
