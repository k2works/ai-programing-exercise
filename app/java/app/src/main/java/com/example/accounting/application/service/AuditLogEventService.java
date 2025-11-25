package com.example.accounting.application.service;

import com.example.accounting.application.port.in.AuditLogEventUseCase;
import com.example.accounting.domain.event.JournalEntryApprovedEvent;
import com.example.accounting.domain.event.JournalEntryCreatedEvent;
import com.example.accounting.domain.event.JournalEntryDeletedEvent;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

/**
 * 監査ログイベントサービス
 *
 * AuditLogUseCase の実装として、仕訳イベントを監査ログに記録します。
 * ヘキサゴナルアーキテクチャにおけるアプリケーションサービスとして、
 * ビジネスロジックを実装します。
 */
@Service
public class AuditLogEventService implements AuditLogEventUseCase {

    private static final Logger logger = LoggerFactory.getLogger(AuditLogEventService.class);

    @Override
    public void logJournalEntryCreated(JournalEntryCreatedEvent event) {
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

    @Override
    public void logJournalEntryApproved(JournalEntryApprovedEvent event) {
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

    @Override
    public void logJournalEntryDeleted(JournalEntryDeletedEvent event) {
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
