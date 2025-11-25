package com.example.accounting.application.port.in;

import com.example.accounting.domain.event.JournalEntryApprovedEvent;
import com.example.accounting.domain.event.JournalEntryCreatedEvent;
import com.example.accounting.domain.event.JournalEntryDeletedEvent;

/**
 * 通知送信ユースケース（入力ポート）
 *
 * 仕訳イベントに基づいて関係者に通知を送信するためのインターフェース。
 * このインターフェースは、イベント駆動アーキテクチャにおける
 * 入力ポートとして機能します。
 */
public interface NotificationEventUseCase {

    /**
     * 仕訳作成イベントに対する通知を送信
     *
     * @param event 仕訳作成イベント
     */
    void notifyJournalEntryCreated(JournalEntryCreatedEvent event);

    /**
     * 仕訳承認イベントに対する通知を送信
     *
     * @param event 仕訳承認イベント
     */
    void notifyJournalEntryApproved(JournalEntryApprovedEvent event);

    /**
     * 仕訳削除イベントに対する通知を送信
     *
     * @param event 仕訳削除イベント
     */
    void notifyJournalEntryDeleted(JournalEntryDeletedEvent event);
}
