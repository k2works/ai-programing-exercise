package com.example.production.infrastructure.out.mapper;

import com.example.production.domain.model.inventory.Stocktaking;
import com.example.production.domain.model.inventory.StocktakingDetail;
import com.example.production.domain.model.inventory.StocktakingStatus;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.math.BigDecimal;
import java.util.List;
import java.util.Optional;

/**
 * 棚卸マッパー
 */
@Mapper
public interface StocktakingMapper {

    void insert(Stocktaking stocktaking);

    void insertDetail(StocktakingDetail detail);

    Optional<Stocktaking> findByStocktakingNumber(@Param("stocktakingNumber") String stocktakingNumber);

    Stocktaking findByStocktakingNumberWithDetails(@Param("stocktakingNumber") String stocktakingNumber);

    List<StocktakingDetail> findDetailsByStocktakingNumber(@Param("stocktakingNumber") String stocktakingNumber);

    void updateDetail(@Param("id") Long id,
                      @Param("actualQuantity") BigDecimal actualQuantity,
                      @Param("differenceQuantity") BigDecimal differenceQuantity);

    void updateStatus(@Param("stocktakingNumber") String stocktakingNumber,
                      @Param("status") StocktakingStatus status);

    long countByYear(@Param("year") int year);

    List<Stocktaking> findAll();

    void deleteAllDetails();

    void deleteAll();
}
