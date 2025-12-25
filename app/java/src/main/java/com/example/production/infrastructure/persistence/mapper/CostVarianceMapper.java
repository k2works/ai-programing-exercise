package com.example.production.infrastructure.persistence.mapper;

import com.example.production.domain.model.cost.CostVariance;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Optional;

/**
 * 原価差異マッパー
 */
@Mapper
public interface CostVarianceMapper {

    void insert(CostVariance costVariance);

    Optional<CostVariance> findById(@Param("id") Integer id);

    Optional<CostVariance> findByWorkOrderNumber(@Param("workOrderNumber") String workOrderNumber);

    List<CostVariance> findByItemCode(@Param("itemCode") String itemCode);

    List<CostVariance> findAll();

    void deleteAll();
}
