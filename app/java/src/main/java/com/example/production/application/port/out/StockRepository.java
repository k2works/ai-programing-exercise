package com.example.production.application.port.out;

import com.example.production.domain.model.inventory.Stock;
import com.example.production.domain.model.inventory.StockStatus;

import java.math.BigDecimal;
import java.util.List;
import java.util.Optional;

/**
 * 在庫リポジトリ（Output Port）
 */
public interface StockRepository {
    void save(Stock stock);
    Optional<Stock> findByLocationAndItem(String locationCode, String itemCode);
    List<Stock> findByLocationCode(String locationCode);
    List<Stock> findByItemCode(String itemCode);
    void increaseByStatus(String locationCode, String itemCode, BigDecimal quantity, StockStatus status);
    void decreaseByStatus(String locationCode, String itemCode, BigDecimal quantity, StockStatus status);
    void changeStatus(String locationCode, String itemCode, BigDecimal quantity, StockStatus fromStatus, StockStatus toStatus);
    void update(Stock stock);
    List<Stock> findAll();
    void deleteAll();
}
