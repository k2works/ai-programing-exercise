package com.example.accounting.application.port.in;

import java.util.List;

/**
 * 比較分析結果
 */
public record ComparativeAnalysisResult(
    List<FinancialAnalysisResult> periods,
    TrendsDto trends
) {}
