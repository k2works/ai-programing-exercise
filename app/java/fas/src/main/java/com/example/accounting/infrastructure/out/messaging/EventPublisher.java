package com.example.accounting.infrastructure.out.messaging;

import com.example.accounting.domain.event.JournalEntryApprovedEvent;
import com.example.accounting.domain.event.JournalEntryCreatedEvent;
import com.example.accounting.domain.event.JournalEntryDeletedEvent;
import com.example.accounting.infrastructure.config.RabbitMQConfig;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.stereotype.Component;

/**
 * イベントパブリッシャー
 *
 * ドメインイベントを RabbitMQ にパブリッシュします。
 * イベント駆動アーキテクチャにおいて、イベントソーシングで保存されたイベントを
 * 他のマイクロサービスに配信する役割を担います。
 */
@Component
@ConditionalOnProperty(name = "rabbitmq.enabled", havingValue = "true", matchIfMissing = false)
public class EventPublisher {

    private static final Logger logger = LoggerFactory.getLogger(EventPublisher.class);

    private final RabbitTemplate rabbitTemplate;

    public EventPublisher(RabbitTemplate rabbitTemplate) {
        this.rabbitTemplate = rabbitTemplate;
    }

    /**
     * イベントをパブリッシュ
     *
     * @param event イベントオブジェクト
     * @param routingKey ルーティングキー
     * @param <T> イベント型
     */
    public <T> void publish(T event, String routingKey) {
        try {
            rabbitTemplate.convertAndSend(routingKey, event);
            logger.info("Event published: routingKey={}, eventType={}",
                routingKey, event.getClass().getSimpleName());
        } catch (Exception e) {
            logger.error("Failed to publish event: routingKey={}", routingKey, e);
            throw new RuntimeException("Failed to publish event", e);
        }
    }

    /**
     * 仕訳作成イベントをパブリッシュ
     *
     * @param event 仕訳作成イベント
     */
    public void publishJournalEntryCreated(JournalEntryCreatedEvent event) {
        publish(event, RabbitMQConfig.ROUTING_KEY_JOURNAL_CREATED);
    }

    /**
     * 仕訳承認イベントをパブリッシュ
     *
     * @param event 仕訳承認イベント
     */
    public void publishJournalEntryApproved(JournalEntryApprovedEvent event) {
        publish(event, RabbitMQConfig.ROUTING_KEY_JOURNAL_APPROVED);
    }

    /**
     * 仕訳削除イベントをパブリッシュ
     *
     * @param event 仕訳削除イベント
     */
    public void publishJournalEntryDeleted(JournalEntryDeletedEvent event) {
        publish(event, RabbitMQConfig.ROUTING_KEY_JOURNAL_DELETED);
    }
}
