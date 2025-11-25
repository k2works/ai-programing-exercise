package com.example.accounting.infrastructure.in.event;

import com.example.accounting.application.port.in.AuditLogEventUseCase;
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
 * 監査ログイベントアダプター（入力アダプター）
 *
 * RabbitMQ からのイベントを受信し、AuditLogUseCase に委譲します。
 * ヘキサゴナルアーキテクチャにおける入力アダプターとして機能し、
 * メッセージングインフラストラクチャとアプリケーション層を分離します。
 */
@Component
@ConditionalOnProperty(name = "rabbitmq.enabled", havingValue = "true", matchIfMissing = false)
public class AuditLogEventAdapter {

    private static final Logger logger = LoggerFactory.getLogger(AuditLogEventAdapter.class);
    private final AuditLogEventUseCase auditLogUseCase;

    public AuditLogEventAdapter(AuditLogEventUseCase auditLogUseCase) {
        this.auditLogUseCase = auditLogUseCase;
    }

    /**
     * 仕訳作成イベントを受信してユースケースに委譲
     *
     * @param event 仕訳作成イベント
     */
    @RabbitListener(queues = RabbitMQConsumerConfig.AUDIT_QUEUE)
    public void handleJournalEntryCreated(JournalEntryCreatedEvent event) {
        logger.debug("【アダプター】仕訳作成イベント受信: journalEntryId={}", event.getJournalEntryId());
        auditLogUseCase.logJournalEntryCreated(event);
    }

    /**
     * 仕訳承認イベントを受信してユースケースに委譲
     *
     * @param event 仕訳承認イベント
     */
    @RabbitListener(queues = RabbitMQConsumerConfig.AUDIT_QUEUE)
    public void handleJournalEntryApproved(JournalEntryApprovedEvent event) {
        logger.debug("【アダプター】仕訳承認イベント受信: journalEntryId={}", event.getJournalEntryId());
        auditLogUseCase.logJournalEntryApproved(event);
    }

    /**
     * 仕訳削除イベントを受信してユースケースに委譲
     *
     * @param event 仕訳削除イベント
     */
    @RabbitListener(queues = RabbitMQConsumerConfig.AUDIT_QUEUE)
    public void handleJournalEntryDeleted(JournalEntryDeletedEvent event) {
        logger.debug("【アダプター】仕訳削除イベント受信: journalEntryId={}", event.getJournalEntryId());
        auditLogUseCase.logJournalEntryDeleted(event);
    }
}
