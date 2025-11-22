package com.example.management.application;

import com.example.management.domain.FinancialAnalysisResult;
import com.example.management.domain.FinancialData;
import com.example.management.domain.FinancialRatios;
import com.example.management.infrastructure.adapter.out.external.FinancialAccountingClient;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.math.RoundingMode;

@Service
public class AnalyzeFinancialDataUseCase {

    private final FinancialAccountingClient financialAccountingClient;

    public AnalyzeFinancialDataUseCase(FinancialAccountingClient financialAccountingClient) {
        this.financialAccountingClient = financialAccountingClient;
    }

    public FinancialAnalysisResult analyze(Integer fiscalYear) {
        // 1. 財務会計サービスからデータ取得
        FinancialData data = financialAccountingClient.fetchFinancialDataByFiscalYear(fiscalYear);

        // 2. 財務比率の計算
        FinancialRatios ratios = calculateRatios(data);

        return new FinancialAnalysisResult(data, ratios);
    }

    private FinancialRatios calculateRatios(FinancialData data) {
        BigDecimal operatingProfitMargin = BigDecimal.ZERO;
        BigDecimal returnOnAssets = BigDecimal.ZERO;
        BigDecimal debtRatio = BigDecimal.ZERO;

        // 営業利益率の計算
        if (data.getSales().compareTo(BigDecimal.ZERO) > 0) {
            operatingProfitMargin = data.getOperatingProfit()
                .divide(data.getSales(), 4, RoundingMode.HALF_UP);
        }

        // 総資産利益率 (ROA) の計算
        if (data.getTotalAssets().compareTo(BigDecimal.ZERO) > 0) {
            returnOnAssets = data.getOperatingProfit()
                .divide(data.getTotalAssets(), 4, RoundingMode.HALF_UP);
        }

        // 負債比率の計算
        if (data.getTotalAssets().compareTo(BigDecimal.ZERO) > 0) {
            debtRatio = data.getTotalLiabilities()
                .divide(data.getTotalAssets(), 4, RoundingMode.HALF_UP);
        }

        return new FinancialRatios(operatingProfitMargin, returnOnAssets, debtRatio);
    }
}
