package com.example.production.infrastructure.out.repository;

import com.example.production.application.port.out.StocktakingRepository;
import com.example.production.domain.model.inventory.Stocktaking;
import com.example.production.domain.model.inventory.StocktakingDetail;
import com.example.production.domain.model.inventory.StocktakingStatus;
import com.example.production.infrastructure.out.mapper.StocktakingMapper;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Repository;

import java.math.BigDecimal;
import java.util.List;
import java.util.Optional;

/**
 * 棚卸リポジトリ実装
 */
@Repository
@RequiredArgsConstructor
public class StocktakingRepositoryImpl implements StocktakingRepository {

    private final StocktakingMapper stocktakingMapper;

    @Override
    public void save(Stocktaking stocktaking) {
        stocktakingMapper.insert(stocktaking);
    }

    @Override
    public void saveDetail(StocktakingDetail detail) {
        stocktakingMapper.insertDetail(detail);
    }

    @Override
    public Optional<Stocktaking> findByStocktakingNumber(String stocktakingNumber) {
        return stocktakingMapper.findByStocktakingNumber(stocktakingNumber);
    }

    @Override
    public Stocktaking findByStocktakingNumberWithDetails(String stocktakingNumber) {
        return stocktakingMapper.findByStocktakingNumberWithDetails(stocktakingNumber);
    }

    @Override
    public List<StocktakingDetail> findDetailsByStocktakingNumber(String stocktakingNumber) {
        return stocktakingMapper.findDetailsByStocktakingNumber(stocktakingNumber);
    }

    @Override
    public void updateDetail(Long id, BigDecimal actualQuantity, BigDecimal differenceQuantity) {
        stocktakingMapper.updateDetail(id, actualQuantity, differenceQuantity);
    }

    @Override
    public void updateStatus(String stocktakingNumber, StocktakingStatus status) {
        stocktakingMapper.updateStatus(stocktakingNumber, status);
    }

    @Override
    public long countByYear(int year) {
        return stocktakingMapper.countByYear(year);
    }

    @Override
    public List<Stocktaking> findAll() {
        return stocktakingMapper.findAll();
    }

    @Override
    public void deleteAll() {
        stocktakingMapper.deleteAll();
    }

    @Override
    public void deleteAllDetails() {
        stocktakingMapper.deleteAllDetails();
    }
}
