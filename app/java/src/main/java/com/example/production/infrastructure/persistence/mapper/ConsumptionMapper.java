package com.example.production.infrastructure.persistence.mapper;

import com.example.production.domain.model.subcontract.Consumption;
import org.apache.ibatis.annotations.Mapper;

import java.util.Optional;

@Mapper
public interface ConsumptionMapper {
    void insert(Consumption consumption);
    Optional<Consumption> findById(Integer id);
    Optional<Consumption> findByConsumptionNumber(String consumptionNumber);
    Optional<Consumption> findByReceivingNumber(String receivingNumber);
    Optional<String> findLatestConsumptionNumber(String prefix);
    void deleteAll();
}
