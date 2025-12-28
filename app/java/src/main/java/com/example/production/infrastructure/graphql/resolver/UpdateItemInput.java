package com.example.production.infrastructure.graphql.resolver;

import com.example.production.domain.model.item.ItemCategory;
import lombok.Data;

/**
 * 品目更新入力
 */
@Data
public class UpdateItemInput {
    private String itemCode;
    private String itemName;
    private ItemCategory category;
    private Integer leadTime;
    private Integer safetyStock;
}
