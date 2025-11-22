package com.example.accounting.application.port.in;

/**
 * 財務分析結果
 */
public record FinancialAnalysisResult(
    int fiscalYear,
    FinancialDataDto financialData,
    RatiosDto ratios
) {}
