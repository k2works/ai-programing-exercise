package com.example.production.infrastructure.in.graphql.resolver;

import com.example.production.domain.model.item.ItemCategory;
import lombok.Data;

import java.math.BigDecimal;

/**
 * 品目登録入力
 */
@Data
public class CreateItemInput {
    private String itemCode;
    private String itemName;
    private ItemCategory category;
    private String itemGroupCode;
    private String unitCode;
    private String locationCode;
    private Integer leadTime;
    private Integer safetyStock;
}
