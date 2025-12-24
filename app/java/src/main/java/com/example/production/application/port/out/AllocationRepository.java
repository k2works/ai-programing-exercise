package com.example.production.application.port.out;

import com.example.production.domain.model.plan.Allocation;

import java.util.List;

/**
 * 引当リポジトリ（Output Port）
 */
public interface AllocationRepository {
    void save(Allocation allocation);
    List<Allocation> findByRequirementId(Integer requirementId);
    void deleteAll();
}
