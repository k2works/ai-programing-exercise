package com.example.production.application.port.in.command;

import com.example.production.domain.model.item.ItemCategory;
import lombok.Builder;
import lombok.Value;

import java.math.BigDecimal;

/**
 * 品目更新コマンド
 */
@Value
@Builder
public class UpdateItemCommand {
    String itemCode;
    String itemName;
    ItemCategory category;
    Integer leadTime;
    Integer safetyLeadTime;
    BigDecimal safetyStock;
    BigDecimal yieldRate;
    BigDecimal minLotSize;
    BigDecimal lotIncrement;
    BigDecimal maxLotSize;
    Integer shelfLife;
}
