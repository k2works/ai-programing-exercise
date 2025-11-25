package com.example.accounting.infrastructure.in.web.dto.management;

/**
 * 財務分析結果
 */
public record FinancialAnalysisResult(
    int fiscalYear,
    FinancialDataDto financialData,
    RatiosDto ratios
) {}
