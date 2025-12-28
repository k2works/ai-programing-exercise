package com.example.production.infrastructure.out.repository;

import com.example.production.application.port.out.CompletionResultRepository;
import com.example.production.domain.model.process.CompletionResult;
import com.example.production.infrastructure.out.mapper.CompletionResultMapper;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

/**
 * 完成実績リポジトリ実装
 */
@Repository
public class CompletionResultRepositoryImpl implements CompletionResultRepository {

    private final CompletionResultMapper completionResultMapper;

    public CompletionResultRepositoryImpl(CompletionResultMapper completionResultMapper) {
        this.completionResultMapper = completionResultMapper;
    }

    @Override
    public void save(CompletionResult completionResult) {
        completionResultMapper.insert(completionResult);
    }

    @Override
    public Optional<CompletionResult> findByCompletionResultNumber(String completionResultNumber) {
        return completionResultMapper.findByCompletionResultNumber(completionResultNumber);
    }

    @Override
    public List<CompletionResult> findByWorkOrderNumber(String workOrderNumber) {
        return completionResultMapper.findByWorkOrderNumber(workOrderNumber);
    }

    @Override
    public Optional<String> findLatestCompletionResultNumber(String prefix) {
        return completionResultMapper.findLatestCompletionResultNumber(prefix);
    }

    @Override
    public List<CompletionResult> findAll() {
        return completionResultMapper.findAll();
    }

    @Override
    public void deleteAll() {
        completionResultMapper.deleteAll();
    }
}
