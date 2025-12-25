package com.example.production.application.port.out;

import com.example.production.domain.model.subcontract.Consumption;

import java.util.Optional;

/**
 * 消費リポジトリ
 */
public interface ConsumptionRepository {
    void save(Consumption consumption);
    Optional<Consumption> findById(Integer id);
    Optional<Consumption> findByConsumptionNumber(String consumptionNumber);
    Optional<Consumption> findByReceivingNumber(String receivingNumber);
    Optional<String> findLatestConsumptionNumber(String prefix);
    void deleteAll();
}
