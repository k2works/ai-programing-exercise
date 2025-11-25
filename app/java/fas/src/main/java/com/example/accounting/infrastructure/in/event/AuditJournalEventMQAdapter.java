package com.example.accounting.infrastructure.in.event;

import com.example.accounting.application.port.in.AuditJournalEventUseCase;
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
 * RabbitMQ からのイベントを受信し、監査ログとして記録します。
 * ヘキサゴナルアーキテクチャにおける入力アダプターとして機能し、
 * メッセージングインフラストラクチャとアプリケーション層を分離します。
 */
@Component
@ConditionalOnProperty(name = "rabbitmq.enabled", havingValue = "true", matchIfMissing = false)
public class AuditJournalEventMQAdapter implements AuditJournalEventUseCase {

    private static final Logger logger = LoggerFactory.getLogger(AuditJournalEventMQAdapter.class);

    /**
     * 仕訳作成イベントを受信して監査ログに記録
     *
     * @param event 仕訳作成イベント
     */
    @Override
    @RabbitListener(queues = RabbitMQConsumerConfig.AUDIT_QUEUE)
    public void handleJournalEntryCreated(JournalEntryCreatedEvent event) {
        logger.info("【監査ログ】仕訳作成: journalEntryId={}, userId={}, entryDate={}, description={}",
            event.getJournalEntryId(),
            event.getUserId(),
            event.getEntryDate(),
            event.getDescription());

        // 監査ログをデータベースに保存
        saveAuditLog(
            "JOURNAL_ENTRY_CREATED",
            event.getJournalEntryId(),
            event.getUserId(),
            String.format("仕訳作成: %s", event.getDescription())
        );
    }

    /**
     * 仕訳承認イベントを受信して監査ログに記録
     *
     * @param event 仕訳承認イベント
     */
    @Override
    @RabbitListener(queues = RabbitMQConsumerConfig.AUDIT_QUEUE)
    public void handleJournalEntryApproved(JournalEntryApprovedEvent event) {
        logger.info("【監査ログ】仕訳承認: journalEntryId={}, approvedBy={}, comment={}",
            event.getJournalEntryId(),
            event.getApprovedBy(),
            event.getApprovalComment());

        // 監査ログをデータベースに保存
        saveAuditLog(
            "JOURNAL_ENTRY_APPROVED",
            event.getJournalEntryId(),
            event.getApprovedBy(),
            String.format("仕訳承認: %s", event.getApprovalComment())
        );
    }

    /**
     * 仕訳削除イベントを受信して監査ログに記録
     *
     * @param event 仕訳削除イベント
     */
    @Override
    @RabbitListener(queues = RabbitMQConsumerConfig.AUDIT_QUEUE)
    public void handleJournalEntryDeleted(JournalEntryDeletedEvent event) {
        logger.info("【監査ログ】仕訳削除: journalEntryId={}, userId={}, reason={}",
            event.getJournalEntryId(),
            event.getUserId(),
            event.getReason());

        // 監査ログをデータベースに保存
        saveAuditLog(
            "JOURNAL_ENTRY_DELETED",
            event.getJournalEntryId(),
            event.getUserId(),
            String.format("仕訳削除: %s", event.getReason())
        );
    }

    /**
     * 監査ログを保存（実装例）
     *
     * @param eventType イベント種別
     * @param journalEntryId 仕訳ID
     * @param userId ユーザーID
     * @param description 説明
     */
    private void saveAuditLog(String eventType, String journalEntryId, String userId, String description) {
        // TODO: 実際には監査ログテーブルに保存
        logger.debug("監査ログ保存: eventType={}, journalEntryId={}, userId={}, description={}",
            eventType, journalEntryId, userId, description);
    }
}
