package com.example.production.application.port.out;

import com.example.production.domain.model.cost.ActualCost;

import java.util.List;
import java.util.Optional;

/**
 * 実際原価リポジトリ
 */
public interface ActualCostRepository {

    void save(ActualCost actualCost);

    void update(ActualCost actualCost);

    Optional<ActualCost> findById(Integer id);

    Optional<ActualCost> findByWorkOrderNumber(String workOrderNumber);

    List<ActualCost> findByItemCode(String itemCode);

    List<ActualCost> findAll();

    void deleteAll();
}
