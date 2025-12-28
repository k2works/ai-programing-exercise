package com.example.production.infrastructure.in.graphql.resolver;

import lombok.Builder;
import lombok.Data;

/**
 * GraphQL ページネーション情報
 */
@Data
@Builder
public class PageInfo {
    private boolean hasNextPage;
    private boolean hasPreviousPage;
    private int totalElements;
    private int totalPages;
}
