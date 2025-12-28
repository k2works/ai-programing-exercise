package com.example.production.infrastructure.in.graphql.resolver;

import lombok.Builder;
import lombok.Data;

import java.math.BigDecimal;

/**
 * 逆展開（使用先照会）結果
 */
@Data
@Builder
public class WhereUsedResult {
    private String parentItemCode;
    private String itemName;
    private BigDecimal requiredQuantity;
    private int level;
}
