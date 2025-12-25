package com.example.production.application.port.out;

import com.example.production.domain.model.process.CompletionInspectionResult;

import java.util.List;

/**
 * 完成検査結果リポジトリ（Output Port）
 */
public interface CompletionInspectionResultRepository {
    void save(CompletionInspectionResult inspectionResult);
    List<CompletionInspectionResult> findByCompletionResultNumber(String completionResultNumber);
    void deleteAll();
}
