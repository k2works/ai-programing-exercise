package com.example.production.infrastructure.out.repository;

import com.example.production.application.port.out.ActualCostRepository;
import com.example.production.domain.model.cost.ActualCost;
import com.example.production.infrastructure.out.mapper.ActualCostMapper;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

/**
 * 実際原価リポジトリ実装
 */
@Repository
@RequiredArgsConstructor
public class ActualCostRepositoryImpl implements ActualCostRepository {

    private final ActualCostMapper actualCostMapper;

    @Override
    public void save(ActualCost actualCost) {
        actualCostMapper.insert(actualCost);
    }

    @Override
    public void update(ActualCost actualCost) {
        actualCostMapper.update(actualCost);
    }

    @Override
    public Optional<ActualCost> findById(Integer id) {
        return actualCostMapper.findById(id);
    }

    @Override
    public Optional<ActualCost> findByWorkOrderNumber(String workOrderNumber) {
        return actualCostMapper.findByWorkOrderNumber(workOrderNumber);
    }

    @Override
    public List<ActualCost> findByItemCode(String itemCode) {
        return actualCostMapper.findByItemCode(itemCode);
    }

    @Override
    public List<ActualCost> findAll() {
        return actualCostMapper.findAll();
    }

    @Override
    public void deleteAll() {
        actualCostMapper.deleteAll();
    }
}
