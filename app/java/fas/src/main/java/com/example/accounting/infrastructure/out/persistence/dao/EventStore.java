package com.example.accounting.infrastructure.out.persistence.dao;

import lombok.Builder;
import lombok.Data;

import java.time.LocalDateTime;

/**
 * イベントストアDAO
 *
 * イベントソーシングパターンのイベントストアテーブルに対応
 */
@Data
@Builder
public class EventStore {
    /**
     * イベントID（グローバルシーケンス番号）
     */
    private Long eventId;

    /**
     * 集約ID（Aggregate の識別子）
     */
    private String aggregateId;

    /**
     * 集約種別（Aggregate の型）
     */
    private String aggregateType;

    /**
     * イベント種別
     */
    private String eventType;

    /**
     * イベントバージョン
     */
    private Integer eventVersion;

    /**
     * イベントデータ（JSONB）
     */
    private String eventData;

    /**
     * 発生日時
     */
    private LocalDateTime occurredAt;

    /**
     * ユーザーID
     */
    private String userId;

    /**
     * 相関ID（関連する一連のイベントをグループ化）
     */
    private String correlationId;

    /**
     * 因果ID（因果関係のあるイベントID）
     */
    private String causationId;

    /**
     * シーケンス番号（Aggregate 内での順序）
     */
    private Integer sequenceNumber;
}
