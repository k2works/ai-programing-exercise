package com.example.management.domain;

/**
 * 財務分析結果
 */
public class FinancialAnalysisResult {
    private FinancialData financialData;
    private FinancialRatios financialRatios;

    public FinancialAnalysisResult(FinancialData financialData, FinancialRatios financialRatios) {
        this.financialData = financialData;
        this.financialRatios = financialRatios;
    }

    // Getters
    public FinancialData getFinancialData() {
        return financialData;
    }

    public FinancialRatios getFinancialRatios() {
        return financialRatios;
    }
}
