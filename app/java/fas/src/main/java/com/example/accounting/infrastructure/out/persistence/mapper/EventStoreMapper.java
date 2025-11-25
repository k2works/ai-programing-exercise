package com.example.accounting.infrastructure.out.persistence.mapper;

import com.example.accounting.infrastructure.out.persistence.dao.EventStore;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.time.LocalDateTime;
import java.util.List;

/**
 * イベントストア Mapper
 */
@Mapper
public interface EventStoreMapper {

    /**
     * イベントを保存
     *
     * @param eventStore イベントストア
     */
    void insert(EventStore eventStore);

    /**
     * 特定の Aggregate のすべてのイベントを取得
     *
     * @param aggregateId 集約ID
     * @return イベントリスト（シーケンス番号順）
     */
    List<EventStore> findByAggregateId(@Param("aggregateId") String aggregateId);

    /**
     * 特定の Aggregate の特定時点までのイベントを取得
     *
     * @param aggregateId 集約ID
     * @param until 時点
     * @return イベントリスト（シーケンス番号順）
     */
    List<EventStore> findByAggregateIdUntil(
        @Param("aggregateId") String aggregateId,
        @Param("until") LocalDateTime until
    );

    /**
     * 特定のイベント種別のイベントを取得
     *
     * @param eventType イベント種別
     * @return イベントリスト
     */
    List<EventStore> findByEventType(@Param("eventType") String eventType);

    /**
     * 相関IDでイベントを取得
     *
     * @param correlationId 相関ID
     * @return イベントリスト
     */
    List<EventStore> findByCorrelationId(@Param("correlationId") String correlationId);

    /**
     * 期間でイベントを取得
     *
     * @param startDate 開始日時
     * @param endDate 終了日時
     * @param limit 取得件数上限
     * @return イベントリスト
     */
    List<EventStore> findByPeriod(
        @Param("startDate") LocalDateTime startDate,
        @Param("endDate") LocalDateTime endDate,
        @Param("limit") int limit
    );

    /**
     * Aggregate の最新シーケンス番号を取得
     *
     * @param aggregateId 集約ID
     * @return 最新シーケンス番号（イベントがない場合は null）
     */
    Integer getLatestSequenceNumber(@Param("aggregateId") String aggregateId);

    /**
     * 全イベント数を取得
     *
     * @return イベント数
     */
    long count();

    /**
     * 特定バージョン以降のイベントを取得
     *
     * @param aggregateId 集約ID
     * @param afterVersion このバージョンより後のイベントを取得
     * @return イベントリスト（シーケンス番号順）
     */
    List<EventStore> findByAggregateIdAfterVersion(
        @Param("aggregateId") String aggregateId,
        @Param("afterVersion") int afterVersion
    );
}
