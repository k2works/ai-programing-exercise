package com.example.accounting.infrastructure.out.persistence.mapper;

import com.example.accounting.infrastructure.out.persistence.dao.AuditLog;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.time.LocalDateTime;
import java.util.List;

/**
 * 監査ログMapper
 */
@Mapper
public interface AuditLogMapper {

    /**
     * 監査ログを登録
     *
     * @param entity 監査ログエンティティ
     */
    void insert(AuditLog entity);

    /**
     * エンティティ種別とIDで監査ログを検索
     *
     * @param entityType エンティティ種別
     * @param entityId エンティティID
     * @return 監査ログリスト（タイムスタンプ降順）
     */
    List<AuditLog> findByEntity(
        @Param("entityType") String entityType,
        @Param("entityId") String entityId
    );

    /**
     * ユーザーIDと期間で監査ログを検索
     *
     * @param userId ユーザーID
     * @param startDate 開始日時
     * @param endDate 終了日時
     * @return 監査ログリスト（タイムスタンプ降順）
     */
    List<AuditLog> findByUser(
        @Param("userId") String userId,
        @Param("startDate") LocalDateTime startDate,
        @Param("endDate") LocalDateTime endDate
    );

    /**
     * 期間で監査ログを検索
     *
     * @param startDate 開始日時
     * @param endDate 終了日時
     * @param limit 取得件数上限
     * @return 監査ログリスト（タイムスタンプ降順）
     */
    List<AuditLog> findByPeriod(
        @Param("startDate") LocalDateTime startDate,
        @Param("endDate") LocalDateTime endDate,
        @Param("limit") int limit
    );

    /**
     * 全ての監査ログを取得
     *
     * @return 監査ログリスト（タイムスタンプ降順）
     */
    List<AuditLog> findAll();
}
