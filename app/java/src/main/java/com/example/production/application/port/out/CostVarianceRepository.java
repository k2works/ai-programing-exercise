package com.example.production.application.port.out;

import com.example.production.domain.model.cost.CostVariance;

import java.util.List;
import java.util.Optional;

/**
 * 原価差異リポジトリ
 */
public interface CostVarianceRepository {

    void save(CostVariance costVariance);

    Optional<CostVariance> findById(Integer id);

    Optional<CostVariance> findByWorkOrderNumber(String workOrderNumber);

    List<CostVariance> findByItemCode(String itemCode);

    List<CostVariance> findAll();

    void deleteAll();
}
