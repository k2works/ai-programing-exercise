package com.example.accounting.application.port.out;

import com.example.accounting.domain.event.DomainEvent;
import com.example.accounting.application.exception.ConcurrentModificationException;

import java.time.LocalDateTime;
import java.util.List;

/**
 * イベントストアリポジトリ
 *
 * イベントソーシングパターンでドメインイベントを永続化
 */
public interface EventStoreRepository {
    /**
     * イベントを保存
     *
     * @param aggregateId 集約ID
     * @param events イベントリスト
     * @param expectedVersion 期待バージョン（楽観的ロック用）
     * @throws ConcurrentModificationException バージョン不一致の場合
     */
    void save(String aggregateId, List<DomainEvent> events, int expectedVersion);

    /**
     * Aggregate のすべてのイベントを取得
     *
     * @param aggregateId 集約ID
     * @return イベントリスト（シーケンス番号順）
     */
    List<DomainEvent> getEvents(String aggregateId);

    /**
     * 特定時点までのイベントを取得
     *
     * @param aggregateId 集約ID
     * @param pointInTime 時点
     * @return イベントリスト（シーケンス番号順）
     */
    List<DomainEvent> getEventsUntil(String aggregateId, LocalDateTime pointInTime);

    /**
     * Aggregate の現在のバージョンを取得
     *
     * @param aggregateId 集約ID
     * @return 現在のバージョン（イベント数）
     */
    int getCurrentVersion(String aggregateId);

    /**
     * 特定バージョン以降のイベントを取得
     *
     * @param aggregateId 集約ID
     * @param afterVersion このバージョンより後のイベントを取得
     * @return イベントリスト
     */
    List<DomainEvent> getEventsAfterVersion(String aggregateId, int afterVersion);
}
