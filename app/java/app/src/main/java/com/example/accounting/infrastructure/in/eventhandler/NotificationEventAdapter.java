package com.example.accounting.infrastructure.in.eventhandler;

import com.example.accounting.application.port.in.NotificationUseCase;
import com.example.accounting.domain.event.JournalEntryApprovedEvent;
import com.example.accounting.domain.event.JournalEntryCreatedEvent;
import com.example.accounting.domain.event.JournalEntryDeletedEvent;
import com.example.accounting.infrastructure.config.RabbitMQConsumerConfig;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.amqp.rabbit.annotation.RabbitListener;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.stereotype.Component;

/**
 * 通知イベントアダプター（入力アダプター）
 *
 * RabbitMQ からのイベントを受信し、NotificationUseCase に委譲します。
 * ヘキサゴナルアーキテクチャにおける入力アダプターとして機能し、
 * メッセージングインフラストラクチャとアプリケーション層を分離します。
 */
@Component
@ConditionalOnProperty(name = "rabbitmq.enabled", havingValue = "true", matchIfMissing = false)
public class NotificationEventAdapter {

    private static final Logger logger = LoggerFactory.getLogger(NotificationEventAdapter.class);
    private final NotificationUseCase notificationUseCase;

    public NotificationEventAdapter(NotificationUseCase notificationUseCase) {
        this.notificationUseCase = notificationUseCase;
    }

    /**
     * 仕訳作成イベントを受信してユースケースに委譲
     *
     * @param event 仕訳作成イベント
     */
    @RabbitListener(queues = RabbitMQConsumerConfig.NOTIFICATION_QUEUE)
    public void handleJournalEntryCreated(JournalEntryCreatedEvent event) {
        logger.debug("【アダプター】仕訳作成イベント受信: journalEntryId={}", event.getJournalEntryId());
        notificationUseCase.notifyJournalEntryCreated(event);
    }

    /**
     * 仕訳承認イベントを受信してユースケースに委譲
     *
     * @param event 仕訳承認イベント
     */
    @RabbitListener(queues = RabbitMQConsumerConfig.NOTIFICATION_QUEUE)
    public void handleJournalEntryApproved(JournalEntryApprovedEvent event) {
        logger.debug("【アダプター】仕訳承認イベント受信: journalEntryId={}", event.getJournalEntryId());
        notificationUseCase.notifyJournalEntryApproved(event);
    }

    /**
     * 仕訳削除イベントを受信してユースケースに委譲
     *
     * @param event 仕訳削除イベント
     */
    @RabbitListener(queues = RabbitMQConsumerConfig.NOTIFICATION_QUEUE)
    public void handleJournalEntryDeleted(JournalEntryDeletedEvent event) {
        logger.debug("【アダプター】仕訳削除イベント受信: journalEntryId={}", event.getJournalEntryId());
        notificationUseCase.notifyJournalEntryDeleted(event);
    }
}
