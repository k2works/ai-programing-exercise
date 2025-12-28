package com.example.production.infrastructure.graphql.resolver;

import lombok.Builder;
import lombok.Data;

import java.math.BigDecimal;
import java.util.List;

/**
 * BOM ツリーノード
 */
@Data
@Builder
public class BomNode {
    private String itemCode;
    private String itemName;
    private BigDecimal requiredQuantity;
    private int level;
    private List<BomNode> children;
}
