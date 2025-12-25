package com.example.production.infrastructure.persistence.repository;

import com.example.production.application.port.out.StandardCostRepository;
import com.example.production.domain.model.cost.StandardCost;
import com.example.production.infrastructure.persistence.mapper.StandardCostMapper;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Repository;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

/**
 * 標準原価リポジトリ実装
 */
@Repository
@RequiredArgsConstructor
public class StandardCostRepositoryImpl implements StandardCostRepository {

    private final StandardCostMapper standardCostMapper;

    @Override
    public void save(StandardCost standardCost) {
        standardCostMapper.insert(standardCost);
    }

    @Override
    public Optional<StandardCost> findById(Integer id) {
        return standardCostMapper.findById(id);
    }

    @Override
    public Optional<StandardCost> findByItemCodeAndDate(String itemCode, LocalDate date) {
        return standardCostMapper.findByItemCodeAndDate(itemCode, date);
    }

    @Override
    public List<StandardCost> findByItemCode(String itemCode) {
        return standardCostMapper.findByItemCode(itemCode);
    }

    @Override
    public List<StandardCost> findAll() {
        return standardCostMapper.findAll();
    }

    @Override
    public void deleteAll() {
        standardCostMapper.deleteAll();
    }
}
