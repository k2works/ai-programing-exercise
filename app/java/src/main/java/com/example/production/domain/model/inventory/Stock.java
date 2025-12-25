package com.example.production.domain.model.inventory;

import com.example.production.domain.model.item.Item;
import com.example.production.domain.model.location.Location;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
import java.time.LocalDateTime;

/**
 * 在庫情報
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class Stock {
    private Integer id;
    private String locationCode;
    private String itemCode;
    private BigDecimal stockQuantity;
    private BigDecimal passedQuantity;
    private BigDecimal defectiveQuantity;
    private BigDecimal uninspectedQuantity;
    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;

    // リレーション
    private Location location;
    private Item item;

    /**
     * 空の在庫を作成する
     */
    public static Stock empty(String locationCode, String itemCode) {
        return Stock.builder()
                .locationCode(locationCode)
                .itemCode(itemCode)
                .stockQuantity(BigDecimal.ZERO)
                .passedQuantity(BigDecimal.ZERO)
                .defectiveQuantity(BigDecimal.ZERO)
                .uninspectedQuantity(BigDecimal.ZERO)
                .build();
    }

    /**
     * 指定した状態の数量を取得する
     */
    public BigDecimal getQuantityByStatus(StockStatus status) {
        return switch (status) {
            case PASSED -> passedQuantity;
            case DEFECTIVE -> defectiveQuantity;
            case UNINSPECTED -> uninspectedQuantity;
        };
    }
}
