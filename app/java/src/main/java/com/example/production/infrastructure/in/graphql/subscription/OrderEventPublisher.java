package com.example.production.infrastructure.in.graphql.subscription;

import com.example.production.domain.model.purchase.PurchaseOrder;
import org.springframework.stereotype.Component;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Sinks;

import java.util.concurrent.ConcurrentHashMap;

/**
 * 発注イベントパブリッシャー
 */
@Component
public class OrderEventPublisher {

    private final Sinks.Many<PurchaseOrder> globalSink =
        Sinks.many().multicast().onBackpressureBuffer();

    private final ConcurrentHashMap<String, Sinks.Many<PurchaseOrder>> orderSinks =
        new ConcurrentHashMap<>();

    /**
     * 特定の発注番号を購読（null の場合は全発注を購読）
     */
    public Flux<PurchaseOrder> subscribe(String orderNumber) {
        if (orderNumber == null) {
            return globalSink.asFlux();
        }

        Sinks.Many<PurchaseOrder> sink = orderSinks.computeIfAbsent(
            orderNumber,
            k -> Sinks.many().multicast().onBackpressureBuffer()
        );
        return sink.asFlux();
    }

    /**
     * ステータス変更を発行
     */
    public void publish(PurchaseOrder order) {
        // グローバルに発行
        globalSink.tryEmitNext(order);

        // 特定の発注番号にも発行
        Sinks.Many<PurchaseOrder> sink = orderSinks.get(order.getPurchaseOrderNumber());
        if (sink != null) {
            sink.tryEmitNext(order);
        }
    }
}
