package com.example.production.application.port.out;

import com.example.production.domain.model.process.Process;

import java.util.List;
import java.util.Optional;

/**
 * 工程リポジトリ（Output Port）
 */
public interface ProcessRepository {
    void save(Process process);
    Optional<Process> findByProcessCode(String processCode);
    List<Process> findAll();
    void deleteAll();
}
