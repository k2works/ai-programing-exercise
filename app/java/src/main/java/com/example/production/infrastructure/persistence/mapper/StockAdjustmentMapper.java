package com.example.production.infrastructure.persistence.mapper;

import com.example.production.domain.model.inventory.StockAdjustment;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * 在庫調整マッパー
 */
@Mapper
public interface StockAdjustmentMapper {

    void insert(StockAdjustment adjustment);

    List<StockAdjustment> findByStocktakingNumber(@Param("stocktakingNumber") String stocktakingNumber);

    void deleteAll();
}
