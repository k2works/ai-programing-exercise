package com.example.accounting.domain.financial;

/**
 * 財務分析結果
 */
public record AnalysisResult(
    Profitability profitability,
    Efficiency efficiency,
    Safety safety
) {}
