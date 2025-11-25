package com.example.accounting.infrastructure.out.persistence.dao;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

/**
 * Aggregate スナップショット DAO
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class AggregateSnapshot {
    /**
     * 集約ID
     */
    private String aggregateId;

    /**
     * 集約種別
     */
    private String aggregateType;

    /**
     * バージョン（スナップショット時点のイベント数）
     */
    private Integer version;

    /**
     * スナップショットデータ（JSON）
     */
    private String snapshotData;

    /**
     * 作成日時
     */
    private LocalDateTime createdAt;
}
