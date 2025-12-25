package com.example.production.infrastructure.persistence.mapper;

import com.example.production.domain.model.cost.ActualCost;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Optional;

/**
 * 実際原価マッパー
 */
@Mapper
public interface ActualCostMapper {

    void insert(ActualCost actualCost);

    void update(ActualCost actualCost);

    Optional<ActualCost> findById(@Param("id") Integer id);

    Optional<ActualCost> findByWorkOrderNumber(@Param("workOrderNumber") String workOrderNumber);

    List<ActualCost> findByItemCode(@Param("itemCode") String itemCode);

    List<ActualCost> findAll();

    void deleteAll();
}
