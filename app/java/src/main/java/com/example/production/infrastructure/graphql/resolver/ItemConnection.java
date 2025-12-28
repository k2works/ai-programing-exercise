package com.example.production.infrastructure.graphql.resolver;

import lombok.Builder;
import lombok.Data;

import java.util.List;

/**
 * 品目一覧（ページネーション付き）
 */
@Data
@Builder
public class ItemConnection {
    private List<ItemEdge> edges;
    private PageInfo pageInfo;
}
