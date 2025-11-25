package com.example.accounting.infrastructure.in.web.controller.management;

import com.example.accounting.infrastructure.in.web.dto.management.ComparativeAnalysisResult;
import com.example.accounting.infrastructure.in.web.dto.management.FinancialAnalysisResult;
import com.example.accounting.application.port.in.FinancialAnalysisUseCase;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * 財務分析 REST API
 */
@RestController
@RequestMapping("/api/v1/financial-analysis")
@RequiredArgsConstructor
@Tag(name = "財務分析", description = "財務分析 API")
public class FinancialAnalysisController {

    private final FinancialAnalysisUseCase financialAnalysisUseCase;

    /**
     * 指定された会計年度の財務分析を取得
     *
     * @param fiscalYear 会計年度
     * @return 財務分析結果
     */
    @Operation(summary = "会計年度別財務分析", description = "指定された会計年度の財務データと財務指標を分析します")
    @GetMapping("/{fiscalYear}")
    public ResponseEntity<FinancialAnalysisResult> analyzeByFiscalYear(
        @PathVariable int fiscalYear
    ) {
        FinancialAnalysisResult result = financialAnalysisUseCase.analyzeByFiscalYear(fiscalYear);
        return ResponseEntity.ok(result);
    }

    /**
     * 複数期間の比較分析を取得
     *
     * @param fiscalYears 会計年度のリスト（カンマ区切り）
     * @return 比較分析結果
     */
    @Operation(summary = "複数期間比較分析", description = "複数の会計年度を比較分析し、トレンドを表示します")
    @GetMapping("/compare")
    public ResponseEntity<ComparativeAnalysisResult> compareMultiplePeriods(
        @RequestParam List<Integer> fiscalYears
    ) {
        ComparativeAnalysisResult result = financialAnalysisUseCase.compareMultiplePeriods(fiscalYears);
        return ResponseEntity.ok(result);
    }
}
