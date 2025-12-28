package com.example.production.infrastructure.in.rest.dto;

import com.example.production.domain.model.item.ItemCategory;
import lombok.Data;

import java.math.BigDecimal;

/**
 * 品目更新リクエスト
 */
@Data
public class UpdateItemRequest {
    private String itemName;
    private ItemCategory category;
    private Integer leadTime;
    private Integer safetyLeadTime;
    private BigDecimal safetyStock;
    private BigDecimal yieldRate;
    private BigDecimal minLotSize;
    private BigDecimal lotIncrement;
    private BigDecimal maxLotSize;
    private Integer shelfLife;
}
