package com.example.production.application.port.out;

import com.example.production.domain.model.inventory.Stocktaking;
import com.example.production.domain.model.inventory.StocktakingDetail;
import com.example.production.domain.model.inventory.StocktakingStatus;

import java.math.BigDecimal;
import java.util.List;
import java.util.Optional;

/**
 * 棚卸リポジトリ
 */
public interface StocktakingRepository {

    void save(Stocktaking stocktaking);

    void saveDetail(StocktakingDetail detail);

    Optional<Stocktaking> findByStocktakingNumber(String stocktakingNumber);

    Stocktaking findByStocktakingNumberWithDetails(String stocktakingNumber);

    List<StocktakingDetail> findDetailsByStocktakingNumber(String stocktakingNumber);

    void updateDetail(Long id, BigDecimal actualQuantity, BigDecimal differenceQuantity);

    void updateStatus(String stocktakingNumber, StocktakingStatus status);

    long countByYear(int year);

    List<Stocktaking> findAll();

    void deleteAll();

    void deleteAllDetails();
}
