package com.example.production.infrastructure.persistence.repository;

import com.example.production.application.port.out.RequirementRepository;
import com.example.production.domain.model.plan.Requirement;
import com.example.production.infrastructure.persistence.mapper.RequirementMapper;
import org.springframework.stereotype.Repository;

import java.math.BigDecimal;
import java.util.List;
import java.util.Optional;

/**
 * 所要リポジトリ実装
 */
@Repository
public class RequirementRepositoryImpl implements RequirementRepository {

    private final RequirementMapper requirementMapper;

    public RequirementRepositoryImpl(RequirementMapper requirementMapper) {
        this.requirementMapper = requirementMapper;
    }

    @Override
    public void save(Requirement requirement) {
        requirementMapper.insert(requirement);
    }

    @Override
    public Optional<Requirement> findById(Integer id) {
        return requirementMapper.findById(id);
    }

    @Override
    public List<Requirement> findByOrderId(Integer orderId) {
        return requirementMapper.findByOrderId(orderId);
    }

    @Override
    public List<Requirement> findAll() {
        return requirementMapper.findAll();
    }

    @Override
    public void updateAllocation(Integer id, BigDecimal allocatedQuantity, BigDecimal shortageQuantity) {
        requirementMapper.updateAllocation(id, allocatedQuantity, shortageQuantity);
    }

    @Override
    public void deleteAll() {
        requirementMapper.deleteAll();
    }
}
