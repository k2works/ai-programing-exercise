package com.example.production.infrastructure.out.repository;

import com.example.production.application.port.out.StockAdjustmentRepository;
import com.example.production.domain.model.inventory.StockAdjustment;
import com.example.production.infrastructure.out.mapper.StockAdjustmentMapper;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Repository;

import java.util.List;

/**
 * 在庫調整リポジトリ実装
 */
@Repository
@RequiredArgsConstructor
public class StockAdjustmentRepositoryImpl implements StockAdjustmentRepository {

    private final StockAdjustmentMapper stockAdjustmentMapper;

    @Override
    public void save(StockAdjustment adjustment) {
        stockAdjustmentMapper.insert(adjustment);
    }

    @Override
    public List<StockAdjustment> findByStocktakingNumber(String stocktakingNumber) {
        return stockAdjustmentMapper.findByStocktakingNumber(stocktakingNumber);
    }

    @Override
    public void deleteAll() {
        stockAdjustmentMapper.deleteAll();
    }
}
