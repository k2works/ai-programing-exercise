package com.example.accounting.application.port.in;

import com.example.accounting.domain.event.JournalEntryApprovedEvent;
import com.example.accounting.domain.event.JournalEntryCreatedEvent;
import com.example.accounting.domain.event.JournalEntryDeletedEvent;

/**
 * 監査ログ記録ユースケース（入力ポート）
 *
 * 仕訳イベントを監査ログとして記録するためのインターフェース。
 * このインターフェースは、イベント駆動アーキテクチャにおける
 * 入力ポートとして機能します。
 */
public interface AuditLogEventUseCase {

    /**
     * 仕訳作成イベントを監査ログに記録
     *
     * @param event 仕訳作成イベント
     */
    void logJournalEntryCreated(JournalEntryCreatedEvent event);

    /**
     * 仕訳承認イベントを監査ログに記録
     *
     * @param event 仕訳承認イベント
     */
    void logJournalEntryApproved(JournalEntryApprovedEvent event);

    /**
     * 仕訳削除イベントを監査ログに記録
     *
     * @param event 仕訳削除イベント
     */
    void logJournalEntryDeleted(JournalEntryDeletedEvent event);
}
