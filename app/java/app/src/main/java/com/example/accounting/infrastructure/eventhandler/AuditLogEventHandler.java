package com.example.accounting.infrastructure.eventhandler;

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
 * 監査ログ用イベントハンドラー
 *
 * 仕訳イベントを監査ログとして記録します。
 * すべての仕訳の作成、承認、削除イベントを受信し、監査証跡を残します。
 */
@Component
@ConditionalOnProperty(name = "rabbitmq.enabled", havingValue = "true", matchIfMissing = false)
public class AuditLogEventHandler {

    private static final Logger logger = LoggerFactory.getLogger(AuditLogEventHandler.class);

    /**
     * 仕訳作成イベントを処理
     *
     * @param event 仕訳作成イベント
     */
    @RabbitListener(queues = RabbitMQConsumerConfig.AUDIT_QUEUE)
    public void handleJournalEntryCreated(JournalEntryCreatedEvent event) {
        logger.info("【監査ログ】仕訳作成: journalEntryId={}, userId={}, entryDate={}, description={}",
            event.getJournalEntryId(),
            event.getUserId(),
            event.getEntryDate(),
            event.getDescription());

        // 監査ログをデータベースに保存
        // TODO: 監査ログリポジトリに保存する実装を追加
        saveAuditLog(
            "JOURNAL_ENTRY_CREATED",
            event.getJournalEntryId(),
            event.getUserId(),
            String.format("仕訳作成: %s", event.getDescription())
        );
    }

    /**
     * 仕訳承認イベントを処理
     *
     * @param event 仕訳承認イベント
     */
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
     * 仕訳削除イベントを処理
     *
     * @param event 仕訳削除イベント
     */
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
