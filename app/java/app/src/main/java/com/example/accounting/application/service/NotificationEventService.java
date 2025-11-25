package com.example.accounting.application.service;

import com.example.accounting.application.port.in.NotificationEventUseCase;
import com.example.accounting.domain.event.JournalEntryApprovedEvent;
import com.example.accounting.domain.event.JournalEntryCreatedEvent;
import com.example.accounting.domain.event.JournalEntryDeletedEvent;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

/**
 * 通知イベントサービス
 *
 * NotificationUseCase の実装として、仕訳イベントに基づいて通知を送信します。
 * ヘキサゴナルアーキテクチャにおけるアプリケーションサービスとして、
 * ビジネスロジックを実装します。
 */
@Service
public class NotificationEventService implements NotificationEventUseCase {

    private static final Logger logger = LoggerFactory.getLogger(NotificationEventService.class);

    @Override
    public void notifyJournalEntryCreated(JournalEntryCreatedEvent event) {
        logger.info("【通知】仕訳作成通知送信: journalEntryId={}, userId={}",
            event.getJournalEntryId(),
            event.getUserId());

        // メール送信
        sendNotification(
            event.getUserId(),
            "仕訳が作成されました",
            String.format(
                "仕訳 %s が作成されました。\n日付: %s\n摘要: %s",
                event.getJournalEntryId(),
                event.getEntryDate(),
                event.getDescription()
            )
        );
    }

    @Override
    public void notifyJournalEntryApproved(JournalEntryApprovedEvent event) {
        logger.info("【通知】仕訳承認通知送信: journalEntryId={}, approvedBy={}",
            event.getJournalEntryId(),
            event.getApprovedBy());

        // メール送信
        sendNotification(
            event.getApprovedBy(),
            "仕訳が承認されました",
            String.format(
                "仕訳 %s が承認されました。\n承認者: %s\nコメント: %s",
                event.getJournalEntryId(),
                event.getApprovedBy(),
                event.getApprovalComment()
            )
        );
    }

    @Override
    public void notifyJournalEntryDeleted(JournalEntryDeletedEvent event) {
        logger.info("【通知】仕訳削除通知送信: journalEntryId={}, userId={}",
            event.getJournalEntryId(),
            event.getUserId());

        // メール送信
        sendNotification(
            event.getUserId(),
            "仕訳が削除されました",
            String.format(
                "仕訳 %s が削除されました。\n削除者: %s\n理由: %s",
                event.getJournalEntryId(),
                event.getUserId(),
                event.getReason()
            )
        );
    }

    /**
     * 通知を送信（実装例）
     *
     * @param userId ユーザーID
     * @param subject 件名
     * @param body 本文
     */
    private void sendNotification(String userId, String subject, String body) {
        // TODO: 実際にはメール送信サービスや通知サービスを呼び出す
        logger.debug("通知送信: userId={}, subject={}, body={}",
            userId, subject, body);
    }
}
