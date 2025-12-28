package com.example.production.infrastructure.out.repository;

import com.example.production.application.port.out.ProcessRepository;
import com.example.production.domain.model.process.Process;
import com.example.production.infrastructure.out.mapper.ProcessMapper;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

/**
 * 工程リポジトリ実装
 */
@Repository
public class ProcessRepositoryImpl implements ProcessRepository {

    private final ProcessMapper processMapper;

    public ProcessRepositoryImpl(ProcessMapper processMapper) {
        this.processMapper = processMapper;
    }

    @Override
    public void save(Process process) {
        processMapper.insert(process);
    }

    @Override
    public Optional<Process> findByProcessCode(String processCode) {
        return processMapper.findByProcessCode(processCode);
    }

    @Override
    public List<Process> findAll() {
        return processMapper.findAll();
    }

    @Override
    public void deleteAll() {
        processMapper.deleteAll();
    }
}
