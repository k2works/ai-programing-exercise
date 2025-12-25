package com.example.production.infrastructure.persistence.mapper;

import com.example.production.domain.model.process.CompletionResult;
import org.apache.ibatis.annotations.Mapper;

import java.util.List;
import java.util.Optional;

@Mapper
public interface CompletionResultMapper {
    void insert(CompletionResult completionResult);
    Optional<CompletionResult> findByCompletionResultNumber(String completionResultNumber);
    List<CompletionResult> findByWorkOrderNumber(String workOrderNumber);
    Optional<String> findLatestCompletionResultNumber(String prefix);
    List<CompletionResult> findAll();
    void deleteAll();
}
