package com.example.production.infrastructure.persistence.repository;

import com.example.production.application.port.out.SupplyDetailRepository;
import com.example.production.domain.model.subcontract.SupplyDetail;
import com.example.production.infrastructure.persistence.mapper.SupplyDetailMapper;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

/**
 * 支給明細リポジトリ実装
 */
@Repository
public class SupplyDetailRepositoryImpl implements SupplyDetailRepository {

    private final SupplyDetailMapper supplyDetailMapper;

    public SupplyDetailRepositoryImpl(SupplyDetailMapper supplyDetailMapper) {
        this.supplyDetailMapper = supplyDetailMapper;
    }

    @Override
    public void save(SupplyDetail detail) {
        supplyDetailMapper.insert(detail);
    }

    @Override
    public Optional<SupplyDetail> findById(Integer id) {
        return supplyDetailMapper.findById(id);
    }

    @Override
    public List<SupplyDetail> findBySupplyNumber(String supplyNumber) {
        return supplyDetailMapper.findBySupplyNumber(supplyNumber);
    }

    @Override
    public void deleteAll() {
        supplyDetailMapper.deleteAll();
    }
}
