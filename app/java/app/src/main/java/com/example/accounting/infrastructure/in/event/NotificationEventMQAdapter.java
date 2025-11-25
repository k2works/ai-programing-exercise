package com.example.accounting.infrastructure.in.event;

import com.example.accounting.application.port.in.NotificationEventUseCase;
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
 * RabbitMQ からのイベントを受信し、通知を送信します。
 * ヘキサゴナルアーキテクチャにおける入力アダプターとして機能し、
 * メッセージングインフラストラクチャとアプリケーション層を分離します。
 */
@Component
@ConditionalOnProperty(name = "rabbitmq.enabled", havingValue = "true", matchIfMissing = false)
public class NotificationEventMQAdapter implements NotificationEventUseCase {

    private static final Logger logger = LoggerFactory.getLogger(NotificationEventMQAdapter.class);

    /**
     * 仕訳作成イベントを受信して通知を送信
     *
     * @param event 仕訳作成イベント
     */
    @Override
    @RabbitListener(queues = RabbitMQConsumerConfig.NOTIFICATION_QUEUE)
    public void handleJournalEntryCreated(JournalEntryCreatedEvent event) {
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

    /**
     * 仕訳承認イベントを受信して通知を送信
     *
     * @param event 仕訳承認イベント
     */
    @Override
    @RabbitListener(queues = RabbitMQConsumerConfig.NOTIFICATION_QUEUE)
    public void handleJournalEntryApproved(JournalEntryApprovedEvent event) {
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

    /**
     * 仕訳削除イベントを受信して通知を送信
     *
     * @param event 仕訳削除イベント
     */
    @Override
    @RabbitListener(queues = RabbitMQConsumerConfig.NOTIFICATION_QUEUE)
    public void handleJournalEntryDeleted(JournalEntryDeletedEvent event) {
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
