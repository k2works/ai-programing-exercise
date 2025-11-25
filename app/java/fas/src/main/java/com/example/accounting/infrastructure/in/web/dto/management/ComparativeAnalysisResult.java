package com.example.accounting.infrastructure.in.web.dto.management;

import java.util.List;

/**
 * 比較分析結果
 */
public record ComparativeAnalysisResult(
    List<FinancialAnalysisResult> periods,
    TrendsDto trends
) {}
