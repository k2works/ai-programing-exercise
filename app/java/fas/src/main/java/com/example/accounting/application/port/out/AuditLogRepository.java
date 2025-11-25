package com.example.accounting.application.port.out;

import com.example.accounting.domain.model.audit.AuditLog;

import java.time.LocalDateTime;
import java.util.List;

/**
 * 監査ログリポジトリ（Output Port）
 */
public interface AuditLogRepository {

    /**
     * 監査ログを保存
     *
     * @param auditLog 監査ログ
     * @return 保存された監査ログ（IDが設定される）
     */
    AuditLog save(AuditLog auditLog);

    /**
     * エンティティ種別とIDで監査ログを検索
     *
     * @param entityType エンティティ種別
     * @param entityId エンティティID
     * @return 監査ログリスト
     */
    List<AuditLog> findByEntity(String entityType, String entityId);

    /**
     * ユーザーIDと期間で監査ログを検索
     *
     * @param userId ユーザーID
     * @param startDate 開始日時
     * @param endDate 終了日時
     * @return 監査ログリスト
     */
    List<AuditLog> findByUser(String userId, LocalDateTime startDate, LocalDateTime endDate);

    /**
     * 期間で監査ログを検索
     *
     * @param startDate 開始日時
     * @param endDate 終了日時
     * @param limit 取得件数上限
     * @return 監査ログリスト
     */
    List<AuditLog> findByPeriod(LocalDateTime startDate, LocalDateTime endDate, int limit);

    /**
     * 全ての監査ログを取得
     *
     * @return 監査ログリスト
     */
    List<AuditLog> findAll();
}
