package com.example.accounting.application.port.in;

import com.example.accounting.domain.event.AccountCreatedEvent;
import com.example.accounting.domain.event.AccountDeletedEvent;
import com.example.accounting.domain.event.AccountUpdatedEvent;
import com.example.accounting.domain.event.JournalCreatedEvent;
import com.example.accounting.domain.event.JournalDeletedEvent;
import com.example.accounting.domain.event.JournalUpdatedEvent;

/**
 * 監査イベントリスナー Port (入力ポート)
 *
 * ドメインイベントを受け取り、監査ログを記録する責務
 */
public interface AuditEventUseCase {
    /**
     * 勘定科目作成イベントをリッスン
     *
     * @param event 勘定科目作成イベント
     */
    void handleAccountCreated(AccountCreatedEvent event);

    /**
     * 勘定科目更新イベントをリッスン
     *
     * @param event 勘定科目更新イベント
     */
    void handleAccountUpdated(AccountUpdatedEvent event);

    /**
     * 勘定科目削除イベントをリッスン
     *
     * @param event 勘定科目削除イベント
     */
    void handleAccountDeleted(AccountDeletedEvent event);

    /**
     * 仕訳作成イベントをリッスン
     *
     * @param event 仕訳作成イベント
     */
    void handleJournalCreated(JournalCreatedEvent event);

    /**
     * 仕訳更新イベントをリッスン
     *
     * @param event 仕訳更新イベント
     */
    void handleJournalUpdated(JournalUpdatedEvent event);

    /**
     * 仕訳削除イベントをリッスン
     *
     * @param event 仕訳削除イベント
     */
    void handleJournalDeleted(JournalDeletedEvent event);
}
