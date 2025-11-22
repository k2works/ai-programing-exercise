package com.example.accounting.infrastructure.out.persistence.mapper;

import com.example.accounting.infrastructure.out.persistence.dao.AggregateSnapshot;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

/**
 * スナップショット Mapper
 */
@Mapper
public interface SnapshotMapper {

    /**
     * スナップショットを挿入
     *
     * @param aggregateId 集約ID
     * @param aggregateType 集約種別
     * @param version バージョン
     * @param snapshotData スナップショットデータ（JSON）
     */
    void insertSnapshot(
        @Param("aggregateId") String aggregateId,
        @Param("aggregateType") String aggregateType,
        @Param("version") int version,
        @Param("snapshotData") String snapshotData
    );

    /**
     * 最新のスナップショットを取得
     *
     * @param aggregateId 集約ID
     * @return スナップショット（存在しない場合は null）
     */
    AggregateSnapshot selectLatestSnapshot(@Param("aggregateId") String aggregateId);

    /**
     * 古いスナップショットを削除（保持数を超えたもの）
     *
     * @param aggregateId 集約ID
     * @param keepCount 保持数
     */
    void deleteOldSnapshots(
        @Param("aggregateId") String aggregateId,
        @Param("keepCount") int keepCount
    );
}
