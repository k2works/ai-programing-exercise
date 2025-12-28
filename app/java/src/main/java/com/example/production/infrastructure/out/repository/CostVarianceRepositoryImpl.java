package com.example.production.infrastructure.out.repository;

import com.example.production.application.port.out.CostVarianceRepository;
import com.example.production.domain.model.cost.CostVariance;
import com.example.production.infrastructure.out.mapper.CostVarianceMapper;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

/**
 * 原価差異リポジトリ実装
 */
@Repository
@RequiredArgsConstructor
public class CostVarianceRepositoryImpl implements CostVarianceRepository {

    private final CostVarianceMapper costVarianceMapper;

    @Override
    public void save(CostVariance costVariance) {
        costVarianceMapper.insert(costVariance);
    }

    @Override
    public Optional<CostVariance> findById(Integer id) {
        return costVarianceMapper.findById(id);
    }

    @Override
    public Optional<CostVariance> findByWorkOrderNumber(String workOrderNumber) {
        return costVarianceMapper.findByWorkOrderNumber(workOrderNumber);
    }

    @Override
    public List<CostVariance> findByItemCode(String itemCode) {
        return costVarianceMapper.findByItemCode(itemCode);
    }

    @Override
    public List<CostVariance> findAll() {
        return costVarianceMapper.findAll();
    }

    @Override
    public void deleteAll() {
        costVarianceMapper.deleteAll();
    }
}
