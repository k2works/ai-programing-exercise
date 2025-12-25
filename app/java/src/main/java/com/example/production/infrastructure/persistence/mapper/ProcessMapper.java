package com.example.production.infrastructure.persistence.mapper;

import com.example.production.domain.model.process.Process;
import org.apache.ibatis.annotations.Mapper;

import java.util.List;
import java.util.Optional;

@Mapper
public interface ProcessMapper {
    void insert(Process process);
    Optional<Process> findByProcessCode(String processCode);
    List<Process> findAll();
    void deleteAll();
}
