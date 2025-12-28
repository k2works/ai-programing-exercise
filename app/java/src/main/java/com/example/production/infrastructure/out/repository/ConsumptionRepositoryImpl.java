package com.example.production.infrastructure.out.repository;

import com.example.production.application.port.out.ConsumptionRepository;
import com.example.production.domain.model.subcontract.Consumption;
import com.example.production.infrastructure.out.mapper.ConsumptionMapper;
import org.springframework.stereotype.Repository;

import java.util.Optional;

/**
 * 消費リポジトリ実装
 */
@Repository
public class ConsumptionRepositoryImpl implements ConsumptionRepository {

    private final ConsumptionMapper consumptionMapper;

    public ConsumptionRepositoryImpl(ConsumptionMapper consumptionMapper) {
        this.consumptionMapper = consumptionMapper;
    }

    @Override
    public void save(Consumption consumption) {
        consumptionMapper.insert(consumption);
    }

    @Override
    public Optional<Consumption> findById(Integer id) {
        return consumptionMapper.findById(id);
    }

    @Override
    public Optional<Consumption> findByConsumptionNumber(String consumptionNumber) {
        return consumptionMapper.findByConsumptionNumber(consumptionNumber);
    }

    @Override
    public Optional<Consumption> findByReceivingNumber(String receivingNumber) {
        return consumptionMapper.findByReceivingNumber(receivingNumber);
    }

    @Override
    public Optional<String> findLatestConsumptionNumber(String prefix) {
        return consumptionMapper.findLatestConsumptionNumber(prefix);
    }

    @Override
    public void deleteAll() {
        consumptionMapper.deleteAll();
    }
}
