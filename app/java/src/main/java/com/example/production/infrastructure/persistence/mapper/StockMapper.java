package com.example.production.infrastructure.persistence.mapper;

import com.example.production.domain.model.inventory.Stock;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.math.BigDecimal;
import java.util.List;
import java.util.Optional;

@Mapper
public interface StockMapper {
    void insert(Stock stock);
    Optional<Stock> findByLocationAndItem(@Param("locationCode") String locationCode, @Param("itemCode") String itemCode);
    List<Stock> findByLocationCode(String locationCode);
    List<Stock> findByItemCode(String itemCode);
    void increaseByStatus(@Param("locationCode") String locationCode,
                          @Param("itemCode") String itemCode,
                          @Param("quantity") BigDecimal quantity,
                          @Param("status") String status);
    void decreaseByStatus(@Param("locationCode") String locationCode,
                          @Param("itemCode") String itemCode,
                          @Param("quantity") BigDecimal quantity,
                          @Param("status") String status);
    void changeStatus(@Param("locationCode") String locationCode,
                      @Param("itemCode") String itemCode,
                      @Param("quantity") BigDecimal quantity,
                      @Param("fromStatus") String fromStatus,
                      @Param("toStatus") String toStatus);
    void update(Stock stock);
    List<Stock> findAll();
    void deleteAll();
}
