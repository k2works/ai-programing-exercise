package com.example.production.infrastructure.persistence.repository;

import com.example.production.application.port.out.UnitRepository;
import com.example.production.domain.model.item.Unit;
import com.example.production.infrastructure.persistence.mapper.UnitMapper;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

/**
 * 単位リポジトリ実装
 */
@Repository
public class UnitRepositoryImpl implements UnitRepository {

    private final UnitMapper unitMapper;

    public UnitRepositoryImpl(UnitMapper unitMapper) {
        this.unitMapper = unitMapper;
    }

    @Override
    public Optional<Unit> findByUnitCode(String unitCode) {
        return Optional.ofNullable(unitMapper.findByUnitCode(unitCode));
    }

    @Override
    public List<Unit> findAll() {
        return unitMapper.findAll();
    }

    @Override
    public void save(Unit unit) {
        unitMapper.insert(unit);
    }

    @Override
    public boolean existsByUnitCode(String unitCode) {
        return unitMapper.existsByUnitCode(unitCode);
    }

    @Override
    public void deleteAll() {
        unitMapper.deleteAll();
    }
}
