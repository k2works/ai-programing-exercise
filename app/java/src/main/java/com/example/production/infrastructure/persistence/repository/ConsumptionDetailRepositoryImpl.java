package com.example.production.infrastructure.persistence.repository;

import com.example.production.application.port.out.ConsumptionDetailRepository;
import com.example.production.domain.model.subcontract.ConsumptionDetail;
import com.example.production.infrastructure.persistence.mapper.ConsumptionDetailMapper;
import org.springframework.stereotype.Repository;

import java.math.BigDecimal;
import java.util.List;
import java.util.Optional;

/**
 * 消費明細リポジトリ実装
 */
@Repository
public class ConsumptionDetailRepositoryImpl implements ConsumptionDetailRepository {

    private final ConsumptionDetailMapper consumptionDetailMapper;

    public ConsumptionDetailRepositoryImpl(ConsumptionDetailMapper consumptionDetailMapper) {
        this.consumptionDetailMapper = consumptionDetailMapper;
    }

    @Override
    public void save(ConsumptionDetail detail) {
        consumptionDetailMapper.insert(detail);
    }

    @Override
    public Optional<ConsumptionDetail> findById(Integer id) {
        return consumptionDetailMapper.findById(id);
    }

    @Override
    public List<ConsumptionDetail> findByConsumptionNumber(String consumptionNumber) {
        return consumptionDetailMapper.findByConsumptionNumber(consumptionNumber);
    }

    @Override
    public BigDecimal sumByPurchaseOrderAndItem(String purchaseOrderNumber, Integer lineNumber, String itemCode) {
        return consumptionDetailMapper.sumByPurchaseOrderAndItem(purchaseOrderNumber, lineNumber, itemCode);
    }

    @Override
    public void deleteAll() {
        consumptionDetailMapper.deleteAll();
    }
}
