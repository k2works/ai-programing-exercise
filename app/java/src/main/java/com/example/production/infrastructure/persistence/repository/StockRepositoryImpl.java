package com.example.production.infrastructure.persistence.repository;

import com.example.production.application.port.out.StockRepository;
import com.example.production.domain.model.inventory.Stock;
import com.example.production.domain.model.inventory.StockStatus;
import com.example.production.infrastructure.persistence.mapper.StockMapper;
import org.springframework.stereotype.Repository;

import java.math.BigDecimal;
import java.util.List;
import java.util.Optional;

/**
 * 在庫リポジトリ実装
 */
@Repository
public class StockRepositoryImpl implements StockRepository {

    private final StockMapper stockMapper;

    public StockRepositoryImpl(StockMapper stockMapper) {
        this.stockMapper = stockMapper;
    }

    @Override
    public void save(Stock stock) {
        stockMapper.insert(stock);
    }

    @Override
    public Optional<Stock> findByLocationAndItem(String locationCode, String itemCode) {
        return stockMapper.findByLocationAndItem(locationCode, itemCode);
    }

    @Override
    public List<Stock> findByLocationCode(String locationCode) {
        return stockMapper.findByLocationCode(locationCode);
    }

    @Override
    public List<Stock> findByItemCode(String itemCode) {
        return stockMapper.findByItemCode(itemCode);
    }

    @Override
    public void increaseByStatus(String locationCode, String itemCode, BigDecimal quantity, StockStatus status) {
        stockMapper.increaseByStatus(locationCode, itemCode, quantity, status.name());
    }

    @Override
    public void decreaseByStatus(String locationCode, String itemCode, BigDecimal quantity, StockStatus status) {
        stockMapper.decreaseByStatus(locationCode, itemCode, quantity, status.name());
    }

    @Override
    public void changeStatus(String locationCode, String itemCode, BigDecimal quantity, StockStatus fromStatus, StockStatus toStatus) {
        stockMapper.changeStatus(locationCode, itemCode, quantity, fromStatus.name(), toStatus.name());
    }

    @Override
    public void update(Stock stock) {
        stockMapper.update(stock);
    }

    @Override
    public List<Stock> findAll() {
        return stockMapper.findAll();
    }

    @Override
    public void deleteAll() {
        stockMapper.deleteAll();
    }
}
