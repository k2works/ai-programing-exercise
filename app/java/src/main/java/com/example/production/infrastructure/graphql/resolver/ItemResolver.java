package com.example.production.infrastructure.graphql.resolver;

import com.example.production.application.port.in.ItemUseCase;
import com.example.production.application.port.in.command.CreateItemCommand;
import com.example.production.application.port.in.command.UpdateItemCommand;
import com.example.production.domain.exception.DuplicateItemException;
import com.example.production.domain.exception.ItemNotFoundException;
import com.example.production.domain.model.item.Item;
import com.example.production.domain.model.item.ItemCategory;
import graphql.GraphQLError;
import graphql.GraphqlErrorBuilder;
import graphql.schema.DataFetchingEnvironment;
import org.springframework.graphql.data.method.annotation.*;
import org.springframework.graphql.execution.ErrorType;
import org.springframework.stereotype.Controller;

import java.math.BigDecimal;
import java.util.List;

/**
 * 品目 GraphQL リゾルバ実装
 * 既存の ItemUseCase を Input Adapter として呼び出す
 */
@Controller
public class ItemResolver {

    private final ItemUseCase itemUseCase;

    public ItemResolver(ItemUseCase itemUseCase) {
        this.itemUseCase = itemUseCase;
    }

    // ========== Query ==========

    /**
     * 品目取得
     */
    @QueryMapping
    public Item item(@Argument String itemCode) {
        try {
            return itemUseCase.getItemByCode(itemCode);
        } catch (ItemNotFoundException e) {
            return null;
        }
    }

    /**
     * 品目一覧（ページネーション付き）
     */
    @QueryMapping
    public ItemConnection items(
            @Argument ItemCategory category,
            @Argument Integer page,
            @Argument Integer size) {

        int pageNum = page != null ? page : 0;
        int pageSize = size != null ? size : 20;

        List<Item> items;
        int totalElements;

        if (category != null) {
            items = itemUseCase.getItemsByCategory(category);
        } else {
            items = itemUseCase.getAllItems();
        }

        totalElements = items.size();

        // ページング処理
        int start = pageNum * pageSize;
        int end = Math.min(start + pageSize, items.size());
        List<Item> pagedItems = start < items.size() ? items.subList(start, end) : List.of();

        return ItemConnection.builder()
            .edges(pagedItems.stream()
                .map(item -> ItemEdge.builder()
                    .node(item)
                    .cursor(item.getItemCode())
                    .build())
                .toList())
            .pageInfo(PageInfo.builder()
                .hasNextPage(end < totalElements)
                .hasPreviousPage(pageNum > 0)
                .totalElements(totalElements)
                .totalPages((int) Math.ceil((double) totalElements / pageSize))
                .build())
            .build();
    }

    // ========== Mutation ==========

    /**
     * 品目登録
     */
    @MutationMapping
    public Item createItem(@Argument CreateItemInput input) {
        CreateItemCommand command = CreateItemCommand.builder()
            .itemCode(input.getItemCode())
            .itemName(input.getItemName())
            .category(input.getCategory())
            .unitCode(input.getUnitCode())
            .leadTime(input.getLeadTime())
            .safetyStock(input.getSafetyStock() != null
                ? BigDecimal.valueOf(input.getSafetyStock())
                : null)
            .build();

        return itemUseCase.createItem(command);
    }

    /**
     * 品目更新
     */
    @MutationMapping
    public Item updateItem(@Argument UpdateItemInput input) {
        UpdateItemCommand command = UpdateItemCommand.builder()
            .itemCode(input.getItemCode())
            .itemName(input.getItemName())
            .category(input.getCategory())
            .leadTime(input.getLeadTime())
            .safetyStock(input.getSafetyStock() != null
                ? BigDecimal.valueOf(input.getSafetyStock())
                : null)
            .build();

        return itemUseCase.updateItem(command);
    }

    /**
     * 品目削除
     */
    @MutationMapping
    public boolean deleteItem(@Argument String itemCode) {
        try {
            itemUseCase.deleteItem(itemCode);
            return true;
        } catch (ItemNotFoundException e) {
            return false;
        }
    }

    // ========== フィールドリゾルバ ==========

    /**
     * Item.category フィールドを itemCategory にマッピング
     */
    @SchemaMapping(typeName = "Item", field = "category")
    public ItemCategory category(Item item) {
        return item.getItemCategory();
    }

    // ========== 例外ハンドリング ==========

    @GraphQlExceptionHandler
    public GraphQLError handleDuplicateItem(DuplicateItemException ex, DataFetchingEnvironment env) {
        return GraphqlErrorBuilder.newError(env)
            .errorType(ErrorType.BAD_REQUEST)
            .message(ex.getMessage())
            .build();
    }

    @GraphQlExceptionHandler
    public GraphQLError handleItemNotFound(ItemNotFoundException ex, DataFetchingEnvironment env) {
        return GraphqlErrorBuilder.newError(env)
            .errorType(ErrorType.NOT_FOUND)
            .message(ex.getMessage())
            .build();
    }
}
