package com.example.production.infrastructure.persistence.repository;

import com.example.production.application.port.out.AllocationRepository;
import com.example.production.domain.model.plan.Allocation;
import com.example.production.infrastructure.persistence.mapper.AllocationMapper;
import org.springframework.stereotype.Repository;

import java.util.List;

/**
 * 引当リポジトリ実装
 */
@Repository
public class AllocationRepositoryImpl implements AllocationRepository {

    private final AllocationMapper allocationMapper;

    public AllocationRepositoryImpl(AllocationMapper allocationMapper) {
        this.allocationMapper = allocationMapper;
    }

    @Override
    public void save(Allocation allocation) {
        allocationMapper.insert(allocation);
    }

    @Override
    public List<Allocation> findByRequirementId(Integer requirementId) {
        return allocationMapper.findByRequirementId(requirementId);
    }

    @Override
    public void deleteAll() {
        allocationMapper.deleteAll();
    }
}
