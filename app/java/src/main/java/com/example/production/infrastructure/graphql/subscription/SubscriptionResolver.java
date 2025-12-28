package com.example.production.infrastructure.graphql.subscription;

import com.example.production.domain.model.purchase.PurchaseOrder;
import lombok.Builder;
import lombok.Data;
import org.springframework.graphql.data.method.annotation.Argument;
import org.springframework.graphql.data.method.annotation.SubscriptionMapping;
import org.springframework.stereotype.Controller;
import reactor.core.publisher.Flux;

import java.math.BigDecimal;
import java.time.Duration;

/**
 * GraphQL Subscription リゾルバ
 */
@Controller
public class SubscriptionResolver {

    private final OrderEventPublisher orderEventPublisher;
    private final MrpProgressPublisher mrpProgressPublisher;

    public SubscriptionResolver(
            OrderEventPublisher orderEventPublisher,
            MrpProgressPublisher mrpProgressPublisher) {
        this.orderEventPublisher = orderEventPublisher;
        this.mrpProgressPublisher = mrpProgressPublisher;
    }

    /**
     * MRP 進捗の購読
     */
    @SubscriptionMapping
    public Flux<MrpProgress> mrpProgress(@Argument String executionId) {
        return mrpProgressPublisher.subscribe(executionId);
    }

    /**
     * 発注ステータス変更の購読
     */
    @SubscriptionMapping
    public Flux<PurchaseOrder> orderStatusChanged(@Argument String orderNumber) {
        return orderEventPublisher.subscribe(orderNumber);
    }

    /**
     * 在庫変動の購読（デモ用にダミーデータを5秒ごとに発行）
     */
    @SubscriptionMapping
    public Flux<StockChange> stockChanged(@Argument String itemCode) {
        return Flux.interval(Duration.ofSeconds(5))
            .map(i -> StockChange.builder()
                .itemCode(itemCode != null ? itemCode : "ITEM-001")
                .changeType(i % 2 == 0 ? "RECEIVE" : "ISSUE")
                .quantity(BigDecimal.valueOf(10 + (i % 5)))
                .build());
    }
}

/**
 * 在庫変動データ
 */
@Data
@Builder
class StockChange {
    private String itemCode;
    private String changeType;
    private BigDecimal quantity;
}
