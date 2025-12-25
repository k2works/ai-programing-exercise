package com.example.production.application.port.out;

import com.example.production.domain.model.inventory.StockAdjustment;

import java.util.List;

/**
 * 在庫調整リポジトリ
 */
public interface StockAdjustmentRepository {

    void save(StockAdjustment adjustment);

    List<StockAdjustment> findByStocktakingNumber(String stocktakingNumber);

    void deleteAll();
}
