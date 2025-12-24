package com.example.production.infrastructure.persistence.mapper;

import com.example.production.domain.model.plan.Allocation;
import org.apache.ibatis.annotations.Mapper;

import java.util.List;

@Mapper
public interface AllocationMapper {
    void insert(Allocation allocation);

    List<Allocation> findByRequirementId(Integer requirementId);

    void deleteAll();
}
