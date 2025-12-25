package com.example.production.application.port.out;

import com.example.production.domain.model.process.CompletionResult;

import java.util.List;
import java.util.Optional;

/**
 * 完成実績リポジトリ（Output Port）
 */
public interface CompletionResultRepository {
    void save(CompletionResult completionResult);
    Optional<CompletionResult> findByCompletionResultNumber(String completionResultNumber);
    List<CompletionResult> findByWorkOrderNumber(String workOrderNumber);
    Optional<String> findLatestCompletionResultNumber(String prefix);
    void deleteAll();
}
