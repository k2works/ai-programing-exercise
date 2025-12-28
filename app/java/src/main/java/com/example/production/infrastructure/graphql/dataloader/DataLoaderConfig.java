package com.example.production.infrastructure.graphql.dataloader;

import com.example.production.application.port.in.ItemUseCase;
import com.example.production.domain.model.item.Item;
import org.springframework.context.annotation.Configuration;
import org.springframework.graphql.execution.BatchLoaderRegistry;
import reactor.core.publisher.Flux;

import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * GraphQL DataLoader 設定
 * N+1 問題を解決するためのバッチローディング
 */
@Configuration
public class DataLoaderConfig {

    private final ItemUseCase itemUseCase;

    public DataLoaderConfig(ItemUseCase itemUseCase, BatchLoaderRegistry registry) {
        this.itemUseCase = itemUseCase;

        // 品目 DataLoader: 複数の品目コードを1回のクエリでバッチ取得
        registry.forTypePair(String.class, Item.class)
            .registerBatchLoader((itemCodes, env) -> {
                // バッチで品目を取得（1回のクエリで複数取得）
                List<Item> items = itemUseCase.getItemsByCodes(itemCodes.stream().toList());

                Map<String, Item> itemMap = items.stream()
                    .collect(Collectors.toMap(Item::getItemCode, Function.identity()));

                // リクエストされた順序で結果を返す
                return Flux.fromIterable(itemCodes)
                    .map(code -> itemMap.get(code));
            });
    }
}
