package com.example.production.infrastructure.in.rest.dto;

import com.example.production.domain.model.item.Item;
import com.example.production.domain.model.item.ItemCategory;
import lombok.Builder;
import lombok.Value;

import java.math.BigDecimal;
import java.time.LocalDate;

/**
 * 品目レスポンス
 */
@Value
@Builder
public class ItemResponse {
    Integer id;
    String itemCode;
    LocalDate effectiveFrom;
    LocalDate effectiveTo;
    String itemName;
    ItemCategory category;
    String unitCode;
    Integer leadTime;
    Integer safetyLeadTime;
    BigDecimal safetyStock;
    BigDecimal yieldRate;
    BigDecimal minLotSize;
    BigDecimal lotIncrement;
    BigDecimal maxLotSize;
    Integer shelfLife;

    public static ItemResponse from(Item item) {
        return ItemResponse.builder()
                .id(item.getId())
                .itemCode(item.getItemCode())
                .effectiveFrom(item.getEffectiveFrom())
                .effectiveTo(item.getEffectiveTo())
                .itemName(item.getItemName())
                .category(item.getItemCategory())
                .unitCode(item.getUnitCode())
                .leadTime(item.getLeadTime())
                .safetyLeadTime(item.getSafetyLeadTime())
                .safetyStock(item.getSafetyStock())
                .yieldRate(item.getYieldRate())
                .minLotSize(item.getMinLotSize())
                .lotIncrement(item.getLotIncrement())
                .maxLotSize(item.getMaxLotSize())
                .shelfLife(item.getShelfLife())
                .build();
    }
}
