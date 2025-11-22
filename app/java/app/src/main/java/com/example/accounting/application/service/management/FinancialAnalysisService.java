package com.example.accounting.application.service.management;

import com.example.accounting.application.port.in.*;
import com.example.accounting.application.port.out.DailyBalanceRepository;
import com.example.accounting.domain.model.management.AnalysisResult;
import com.example.accounting.domain.model.management.FinancialData;
import com.example.accounting.domain.model.management.FinancialRatioAnalyzer;
import com.example.accounting.domain.model.financial.DailyBalance;
import com.example.accounting.infrastructure.in.web.dto.management.*;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;

/**
 * 財務分析サービス
 *
 * 日次残高データから財務データを集計し、財務指標を計算します。
 */
@Service
@RequiredArgsConstructor
@Transactional(readOnly = true)
public class FinancialAnalysisService implements FinancialAnalysisUseCase {

    private final DailyBalanceRepository dailyBalanceRepository;
    private final FinancialRatioAnalyzer analyzer = new FinancialRatioAnalyzer();

    @Override
    public FinancialAnalysisResult analyzeByFiscalYear(int fiscalYear) {
        // 会計年度の期末日を計算（3月31日）
        LocalDate fiscalYearEnd = LocalDate.of(fiscalYear + 1, 3, 31);

        // 期末日の日次残高データを取得
        List<DailyBalance> balances = dailyBalanceRepository.findByDate(fiscalYearEnd);

        if (balances.isEmpty()) {
            throw new IllegalArgumentException(
                String.format("会計年度 %d の財務データが見つかりません", fiscalYear)
            );
        }

        // 日次残高から仕訳明細データに変換
        List<FinancialData.JournalEntryData> entries = balances.stream()
            .map(balance -> new FinancialData.JournalEntryData(
                balance.getAccountCode(),
                balance.getDebitAmount(),
                balance.getCreditAmount()
            ))
            .toList();

        // 財務データを生成
        FinancialData financialData = FinancialData.fromJournalEntries(fiscalYear, entries);

        // 財務分析を実行
        AnalysisResult ratios = analyzer.analyze(financialData);

        // 結果を DTO に変換
        return new FinancialAnalysisResult(
            fiscalYear,
            toFinancialDataDto(financialData),
            toRatiosDto(ratios)
        );
    }

    @Override
    public ComparativeAnalysisResult compareMultiplePeriods(List<Integer> fiscalYears) {
        // 各期間の分析を実行
        List<FinancialAnalysisResult> periods = fiscalYears.stream()
            .map(this::analyzeByFiscalYear)
            .toList();

        if (periods.size() < 2) {
            return new ComparativeAnalysisResult(
                periods,
                new TrendsDto(BigDecimal.ZERO, BigDecimal.ZERO, BigDecimal.ZERO)
            );
        }

        // 最初の期間と最後の期間の差分を計算
        FinancialAnalysisResult first = periods.get(0);
        FinancialAnalysisResult last = periods.get(periods.size() - 1);

        TrendsDto trends = new TrendsDto(
            last.ratios().profitability().operatingProfitMargin()
                .subtract(first.ratios().profitability().operatingProfitMargin()),
            last.ratios().efficiency().totalAssetTurnover()
                .subtract(first.ratios().efficiency().totalAssetTurnover()),
            last.ratios().safety().equityRatio()
                .subtract(first.ratios().safety().equityRatio())
        );

        return new ComparativeAnalysisResult(periods, trends);
    }

    /**
     * FinancialData を DTO に変換
     */
    private FinancialDataDto toFinancialDataDto(FinancialData data) {
        return new FinancialDataDto(
            data.sales(),
            data.operatingProfit(),
            data.totalAssets(),
            data.tangibleFixedAssets(),
            data.currentAssets(),
            data.currentLiabilities(),
            data.quickAssets(),
            data.equity()
        );
    }

    /**
     * AnalysisResult を DTO に変換
     */
    private RatiosDto toRatiosDto(AnalysisResult ratios) {
        return new RatiosDto(
            new ProfitabilityDto(ratios.profitability().operatingProfitMargin()),
            new EfficiencyDto(
                ratios.efficiency().totalAssetTurnover(),
                ratios.efficiency().tangibleFixedAssetTurnover()
            ),
            new SafetyDto(
                ratios.safety().currentRatio(),
                ratios.safety().quickRatio(),
                ratios.safety().equityRatio()
            )
        );
    }
}
