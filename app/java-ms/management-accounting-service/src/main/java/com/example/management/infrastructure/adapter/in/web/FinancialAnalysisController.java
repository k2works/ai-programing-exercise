package com.example.management.infrastructure.adapter.in.web;

import com.example.management.application.AnalyzeFinancialDataUseCase;
import com.example.management.domain.FinancialAnalysisResult;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/v1/financial-analysis")
public class FinancialAnalysisController {

    private final AnalyzeFinancialDataUseCase analyzeFinancialDataUseCase;

    public FinancialAnalysisController(AnalyzeFinancialDataUseCase analyzeFinancialDataUseCase) {
        this.analyzeFinancialDataUseCase = analyzeFinancialDataUseCase;
    }

    @GetMapping("/{fiscalYear}")
    public ResponseEntity<FinancialAnalysisResult> analyzeByFiscalYear(@PathVariable Integer fiscalYear) {
        FinancialAnalysisResult result = analyzeFinancialDataUseCase.analyze(fiscalYear);
        return ResponseEntity.ok(result);
    }
}
