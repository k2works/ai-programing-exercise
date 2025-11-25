package com.example.accounting.application.port.out;

import com.example.accounting.domain.event.JournalEntryApprovedEvent;
import com.example.accounting.domain.event.JournalEntryCreatedEvent;
import com.example.accounting.domain.event.JournalEntryDeletedEvent;

/**
 * 仕訳 Projection Port (出力ポート)
 *
 * イベントストアから Read Model を構築する責務
 */
public interface JournalEntryProjection {
    /**
     * 仕訳作成イベントハンドラー
     *
     * @param event 仕訳作成イベント
     */
    void handleJournalEntryCreated(JournalEntryCreatedEvent event);

    /**
     * 仕訳承認イベントハンドラー
     *
     * @param event 仕訳承認イベント
     */
    void handleJournalEntryApproved(JournalEntryApprovedEvent event);

    /**
     * 仕訳削除イベントハンドラー
     *
     * @param event 仕訳削除イベント
     */
    void handleJournalEntryDeleted(JournalEntryDeletedEvent event);
}
