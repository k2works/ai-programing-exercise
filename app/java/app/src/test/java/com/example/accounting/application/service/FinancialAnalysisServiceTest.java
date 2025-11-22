package com.example.accounting.application.service;

import com.example.accounting.infrastructure.in.web.dto.management.ComparativeAnalysisResult;
import com.example.accounting.infrastructure.in.web.dto.management.FinancialAnalysisResult;
import com.example.accounting.application.port.out.DailyBalanceRepository;
import com.example.accounting.application.service.management.FinancialAnalysisService;
import com.example.accounting.domain.model.DailyBalance;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;

import static org.assertj.core.api.Assertions.*;
import static org.mockito.Mockito.when;

/**
 * FinancialAnalysisService のテスト
 */
@ExtendWith(MockitoExtension.class)
@DisplayName("FinancialAnalysisService - 財務分析サービス")
class FinancialAnalysisServiceTest {

    @Mock
    private DailyBalanceRepository dailyBalanceRepository;

    private FinancialAnalysisService service;

    @BeforeEach
    void setUp() {
        service = new FinancialAnalysisService(dailyBalanceRepository);
    }

    @Test
    @DisplayName("会計年度を指定して財務分析を実行できること")
    void shouldAnalyzeByFiscalYear() {
        // Given: 令和3年度の期末残高データ（2022年3月31日）
        LocalDate fiscalYearEnd = LocalDate.of(2022, 3, 31);
        List<DailyBalance> balances = createFY2021Balances(fiscalYearEnd);

        when(dailyBalanceRepository.findByDate(fiscalYearEnd)).thenReturn(balances);

        // When: 財務分析を実行
        FinancialAnalysisResult result = service.analyzeByFiscalYear(2021);

        // Then: 正しい結果が返ること
        assertThat(result.fiscalYear()).isEqualTo(2021);
        assertThat(result.financialData().sales()).isEqualByComparingTo(new BigDecimal("5796105000"));
        assertThat(result.ratios().profitability().operatingProfitMargin())
            .isCloseTo(new BigDecimal("16.99"), within(new BigDecimal("0.01")));
    }

    @Test
    @DisplayName("データが見つからない場合は例外をスローすること")
    void shouldThrowExceptionWhenDataNotFound() {
        // Given: データが見つからない
        LocalDate fiscalYearEnd = LocalDate.of(10000, 3, 31);
        when(dailyBalanceRepository.findByDate(fiscalYearEnd)).thenReturn(List.of());

        // When & Then
        assertThatThrownBy(() -> service.analyzeByFiscalYear(9999))
            .isInstanceOf(IllegalArgumentException.class)
            .hasMessageContaining("会計年度 9999 の財務データが見つかりません");
    }

    @Test
    @DisplayName("複数期間の比較分析を実行できること")
    void shouldCompareMultiplePeriods() {
        // Given: 令和3年度と令和4年度のデータ
        LocalDate fy2021End = LocalDate.of(2022, 3, 31);
        LocalDate fy2022End = LocalDate.of(2023, 3, 31);

        when(dailyBalanceRepository.findByDate(fy2021End))
            .thenReturn(createFY2021Balances(fy2021End));
        when(dailyBalanceRepository.findByDate(fy2022End))
            .thenReturn(createFY2022Balances(fy2022End));

        // When: 比較分析を実行
        ComparativeAnalysisResult result = service.compareMultiplePeriods(List.of(2021, 2022));

        // Then: トレンドが計算されること
        assertThat(result.periods()).hasSize(2);
        assertThat(result.trends().operatingProfitMarginChange())
            .isLessThan(BigDecimal.ZERO);  // 営業利益率は減少
    }

    @Test
    @DisplayName("単一期間の比較分析でもエラーにならないこと")
    void shouldHandleSinglePeriodComparison() {
        // Given
        LocalDate fy2021End = LocalDate.of(2022, 3, 31);
        when(dailyBalanceRepository.findByDate(fy2021End))
            .thenReturn(createFY2021Balances(fy2021End));

        // When
        ComparativeAnalysisResult result = service.compareMultiplePeriods(List.of(2021));

        // Then
        assertThat(result.periods()).hasSize(1);
        assertThat(result.trends().operatingProfitMarginChange())
            .isEqualByComparingTo(BigDecimal.ZERO);
    }

    // テストヘルパーメソッド

    private List<DailyBalance> createFY2021Balances(LocalDate date) {
        return List.of(
            createBalance(date, "11", new BigDecimal("2676193000"), BigDecimal.ZERO),
            createBalance(date, "12", new BigDecimal("186973000"), BigDecimal.ZERO),
            createBalance(date, "21", BigDecimal.ZERO, new BigDecimal("851394000")),
            createBalance(date, "31", BigDecimal.ZERO, new BigDecimal("100000000")),
            createBalance(date, "33", BigDecimal.ZERO, new BigDecimal("1889272000")),
            createBalance(date, "41", BigDecimal.ZERO, new BigDecimal("5796105000")),
            createBalance(date, "51", new BigDecimal("2185856000"), BigDecimal.ZERO),
            createBalance(date, "52", new BigDecimal("2625222000"), BigDecimal.ZERO),
            createBalance(date, "114", new BigDecimal("500000000"), BigDecimal.ZERO),
            createBalance(date, "121", new BigDecimal("64500000"), BigDecimal.ZERO)
        );
    }

    private List<DailyBalance> createFY2022Balances(LocalDate date) {
        return List.of(
            createBalance(date, "11", new BigDecimal("2777545000"), BigDecimal.ZERO),
            createBalance(date, "12", new BigDecimal("197354000"), BigDecimal.ZERO),
            createBalance(date, "21", BigDecimal.ZERO, new BigDecimal("640513000")),
            createBalance(date, "31", BigDecimal.ZERO, new BigDecimal("100000000")),
            createBalance(date, "33", BigDecimal.ZERO, new BigDecimal("2207233000")),
            createBalance(date, "41", BigDecimal.ZERO, new BigDecimal("4547908000")),
            createBalance(date, "51", new BigDecimal("1743821000"), BigDecimal.ZERO),
            createBalance(date, "52", new BigDecimal("2277050000"), BigDecimal.ZERO),
            createBalance(date, "114", new BigDecimal("400000000"), BigDecimal.ZERO),
            createBalance(date, "121", new BigDecimal("50000000"), BigDecimal.ZERO)
        );
    }

    private DailyBalance createBalance(
        LocalDate date,
        String accountCode,
        BigDecimal debitAmount,
        BigDecimal creditAmount
    ) {
        DailyBalance balance = new DailyBalance();
        balance.setEntryDate(date);
        balance.setAccountCode(accountCode);
        balance.setDebitAmount(debitAmount);
        balance.setCreditAmount(creditAmount);
        return balance;
    }
}
