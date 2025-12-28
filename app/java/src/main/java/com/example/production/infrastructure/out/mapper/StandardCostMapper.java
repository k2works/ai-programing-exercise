package com.example.production.infrastructure.out.mapper;

import com.example.production.domain.model.cost.StandardCost;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

/**
 * 標準原価マッパー
 */
@Mapper
public interface StandardCostMapper {

    void insert(StandardCost standardCost);

    Optional<StandardCost> findById(@Param("id") Integer id);

    Optional<StandardCost> findByItemCodeAndDate(
            @Param("itemCode") String itemCode,
            @Param("date") LocalDate date);

    List<StandardCost> findByItemCode(@Param("itemCode") String itemCode);

    List<StandardCost> findAll();

    void deleteAll();
}
