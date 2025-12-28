package com.example.production.infrastructure.out.repository;

import com.example.production.application.port.out.CompletionInspectionResultRepository;
import com.example.production.domain.model.process.CompletionInspectionResult;
import com.example.production.infrastructure.out.mapper.CompletionInspectionResultMapper;
import org.springframework.stereotype.Repository;

import java.util.List;

/**
 * 完成検査結果リポジトリ実装
 */
@Repository
public class CompletionInspectionResultRepositoryImpl implements CompletionInspectionResultRepository {

    private final CompletionInspectionResultMapper completionInspectionResultMapper;

    public CompletionInspectionResultRepositoryImpl(CompletionInspectionResultMapper completionInspectionResultMapper) {
        this.completionInspectionResultMapper = completionInspectionResultMapper;
    }

    @Override
    public void save(CompletionInspectionResult inspectionResult) {
        completionInspectionResultMapper.insert(inspectionResult);
    }

    @Override
    public List<CompletionInspectionResult> findByCompletionResultNumber(String completionResultNumber) {
        return completionInspectionResultMapper.findByCompletionResultNumber(completionResultNumber);
    }

    @Override
    public void deleteAll() {
        completionInspectionResultMapper.deleteAll();
    }
}
