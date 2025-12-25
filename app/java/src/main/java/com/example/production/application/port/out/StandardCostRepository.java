package com.example.production.application.port.out;

import com.example.production.domain.model.cost.StandardCost;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

/**
 * 標準原価リポジトリ
 */
public interface StandardCostRepository {

    void save(StandardCost standardCost);

    Optional<StandardCost> findById(Integer id);

    Optional<StandardCost> findByItemCodeAndDate(String itemCode, LocalDate date);

    List<StandardCost> findByItemCode(String itemCode);

    List<StandardCost> findAll();

    void deleteAll();
}
