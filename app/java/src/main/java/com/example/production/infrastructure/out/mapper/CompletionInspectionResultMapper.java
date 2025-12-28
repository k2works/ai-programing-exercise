package com.example.production.infrastructure.out.mapper;

import com.example.production.domain.model.process.CompletionInspectionResult;
import org.apache.ibatis.annotations.Mapper;

import java.util.List;

@Mapper
public interface CompletionInspectionResultMapper {
    void insert(CompletionInspectionResult inspectionResult);
    List<CompletionInspectionResult> findByCompletionResultNumber(String completionResultNumber);
    void deleteAll();
}
