package com.example.accounting.application.port.in;

import com.example.accounting.infrastructure.in.web.dto.management.ComparativeAnalysisResult;
import com.example.accounting.infrastructure.in.web.dto.management.FinancialAnalysisResult;

import java.util.List;

/**
 * 財務分析のユースケースを定義するインターフェース（Input Port）
 */
public interface FinancialAnalysisUseCase {

    /**
     * 指定された会計年度の財務分析を実行
     *
     * @param fiscalYear 会計年度
     * @return 財務分析結果
     */
    FinancialAnalysisResult analyzeByFiscalYear(int fiscalYear);

    /**
     * 複数期間の財務分析を比較
     *
     * @param fiscalYears 会計年度のリスト
     * @return 比較分析結果
     */
    ComparativeAnalysisResult compareMultiplePeriods(List<Integer> fiscalYears);
}
