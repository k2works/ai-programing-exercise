package com.example.accounting.infrastructure.config;

import org.springframework.amqp.core.*;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * RabbitMQ Consumer 設定
 *
 * イベントを受信するためのキューとバインディングを定義します。
 * 監査サービスと通知サービス用のキューを作成し、
 * Topic Exchange からルーティングキーパターンでイベントを受信します。
 */
@Configuration
@ConditionalOnProperty(name = "rabbitmq.enabled", havingValue = "true", matchIfMissing = false)
public class RabbitMQConsumerConfig {

    public static final String AUDIT_QUEUE = "audit-service-queue";
    public static final String NOTIFICATION_QUEUE = "notification-service-queue";

    /**
     * 監査サービス用キュー
     *
     * durable: サーバー再起動後もキューが永続化される
     */
    @Bean
    public Queue auditQueue() {
        return QueueBuilder
            .durable(AUDIT_QUEUE)
            .build();
    }

    /**
     * 通知サービス用キュー
     *
     * durable: サーバー再起動後もキューが永続化される
     */
    @Bean
    public Queue notificationQueue() {
        return QueueBuilder
            .durable(NOTIFICATION_QUEUE)
            .build();
    }

    /**
     * キューと Exchange のバインディング（監査サービス）
     *
     * "financial.journalentry.*" パターンで全ての仕訳イベントを受信
     * - financial.journalentry.created
     * - financial.journalentry.approved
     * - financial.journalentry.deleted
     */
    @Bean
    public Binding auditQueueBinding(Queue auditQueue, TopicExchange financialEventsExchange) {
        return BindingBuilder
            .bind(auditQueue)
            .to(financialEventsExchange)
            .with("financial.journalentry.*");
    }

    /**
     * キューと Exchange のバインディング（通知サービス）
     *
     * "financial.journalentry.*" パターンで全ての仕訳イベントを受信
     */
    @Bean
    public Binding notificationQueueBinding(Queue notificationQueue, TopicExchange financialEventsExchange) {
        return BindingBuilder
            .bind(notificationQueue)
            .to(financialEventsExchange)
            .with("financial.journalentry.*");
    }
}
