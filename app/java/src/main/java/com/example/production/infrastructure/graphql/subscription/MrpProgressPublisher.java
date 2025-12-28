package com.example.production.infrastructure.graphql.subscription;

import lombok.Builder;
import lombok.Data;
import org.springframework.stereotype.Component;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Sinks;

import java.util.concurrent.ConcurrentHashMap;

/**
 * MRP 進捗イベントパブリッシャー
 */
@Component
public class MrpProgressPublisher {

    private final ConcurrentHashMap<String, Sinks.Many<MrpProgress>> sinks =
        new ConcurrentHashMap<>();

    /**
     * 進捗を購読
     */
    public Flux<MrpProgress> subscribe(String executionId) {
        Sinks.Many<MrpProgress> sink = sinks.computeIfAbsent(
            executionId,
            k -> Sinks.many().multicast().onBackpressureBuffer()
        );
        return sink.asFlux();
    }

    /**
     * 進捗を発行
     */
    public void publish(String executionId, MrpProgress progress) {
        Sinks.Many<MrpProgress> sink = sinks.get(executionId);
        if (sink != null) {
            sink.tryEmitNext(progress);
        }
    }

    /**
     * 完了
     */
    public void complete(String executionId) {
        Sinks.Many<MrpProgress> sink = sinks.remove(executionId);
        if (sink != null) {
            sink.tryEmitComplete();
        }
    }
}

/**
 * MRP 進捗データ
 */
@Data
@Builder
class MrpProgress {
    private String executionId;
    private String phase;
    private int current;
    private int total;
    private String message;
}
