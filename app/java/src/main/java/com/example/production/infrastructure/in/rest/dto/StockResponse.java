package com.example.production.infrastructure.in.rest.dto;

import com.example.production.domain.model.inventory.Stock;
import lombok.Builder;
import lombok.Value;

import java.math.BigDecimal;

/**
 * 在庫レスポンス
 */
@Value
@Builder
public class StockResponse {
    String locationCode;
    String itemCode;
    BigDecimal stockQuantity;
    BigDecimal passedQuantity;
    BigDecimal defectiveQuantity;
    BigDecimal uninspectedQuantity;

    public static StockResponse from(Stock stock) {
        return StockResponse.builder()
                .locationCode(stock.getLocationCode())
                .itemCode(stock.getItemCode())
                .stockQuantity(stock.getStockQuantity())
                .passedQuantity(stock.getPassedQuantity())
                .defectiveQuantity(stock.getDefectiveQuantity())
                .uninspectedQuantity(stock.getUninspectedQuantity())
                .build();
    }
}
