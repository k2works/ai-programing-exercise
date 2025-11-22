package com.example.accounting.domain.repository;

import com.example.accounting.domain.aggregate.JournalEntryAggregate;

import java.util.Optional;

/**
 * スナップショットリポジトリ
 *
 * イベント再生の最適化のためにスナップショットを管理
 */
public interface SnapshotRepository {
    /**
     * スナップショットを保存
     *
     * @param aggregateId 集約ID
     * @param version バージョン
     * @param aggregate Aggregate
     */
    void saveSnapshot(String aggregateId, int version, JournalEntryAggregate aggregate);

    /**
     * 最新のスナップショットを取得
     *
     * @param aggregateId 集約ID
     * @return スナップショット（存在しない場合は空）
     */
    Optional<Snapshot> getLatestSnapshot(String aggregateId);

    /**
     * スナップショット
     *
     * @param aggregateId 集約ID
     * @param version バージョン
     * @param aggregate Aggregate
     */
    record Snapshot(
        String aggregateId,
        int version,
        JournalEntryAggregate aggregate
    ) {}
}
