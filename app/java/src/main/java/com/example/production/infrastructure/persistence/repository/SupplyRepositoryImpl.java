package com.example.production.infrastructure.persistence.repository;

import com.example.production.application.port.out.SupplyRepository;
import com.example.production.domain.model.subcontract.Supply;
import com.example.production.infrastructure.persistence.mapper.SupplyMapper;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

/**
 * 支給リポジトリ実装
 */
@Repository
public class SupplyRepositoryImpl implements SupplyRepository {

    private final SupplyMapper supplyMapper;

    public SupplyRepositoryImpl(SupplyMapper supplyMapper) {
        this.supplyMapper = supplyMapper;
    }

    @Override
    public void save(Supply supply) {
        supplyMapper.insert(supply);
    }

    @Override
    public Optional<Supply> findById(Integer id) {
        return supplyMapper.findById(id);
    }

    @Override
    public Optional<Supply> findBySupplyNumber(String supplyNumber) {
        return supplyMapper.findBySupplyNumber(supplyNumber);
    }

    @Override
    public List<Supply> findByPurchaseOrderDetail(String purchaseOrderNumber, Integer lineNumber) {
        return supplyMapper.findByPurchaseOrderDetail(purchaseOrderNumber, lineNumber);
    }

    @Override
    public Optional<String> findLatestSupplyNumber(String prefix) {
        return supplyMapper.findLatestSupplyNumber(prefix);
    }

    @Override
    public void deleteAll() {
        supplyMapper.deleteAll();
    }
}
