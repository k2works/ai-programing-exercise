package com.example.production.infrastructure.graphql.resolver;

import com.example.production.domain.model.item.Item;
import lombok.Builder;
import lombok.Data;

/**
 * 品目エッジ（Relay スタイル）
 */
@Data
@Builder
public class ItemEdge {
    private Item node;
    private String cursor;
}
