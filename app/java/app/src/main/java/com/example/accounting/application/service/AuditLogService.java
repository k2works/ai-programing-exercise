package com.example.accounting.application.service;

import com.example.accounting.application.port.out.AuditLogRepository;
import com.example.accounting.domain.model.audit.AuditLog;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.List;

/**
 * 監査ログサービス
 */
@Service
@Transactional(readOnly = true)
public class AuditLogService {

    private final AuditLogRepository auditLogRepository;

    public AuditLogService(AuditLogRepository auditLogRepository) {
        this.auditLogRepository = auditLogRepository;
    }

    /**
     * 監査ログを記録
     *
     * @param auditLog 監査ログ
     * @return 記録された監査ログ
     */
    @Transactional
    public AuditLog recordLog(AuditLog auditLog) {
        return auditLogRepository.save(auditLog);
    }

    /**
     * エンティティの監査ログを取得
     *
     * @param entityType エンティティ種別
     * @param entityId エンティティID
     * @return 監査ログリスト
     */
    public List<AuditLog> getLogsByEntity(String entityType, String entityId) {
        return auditLogRepository.findByEntity(entityType, entityId);
    }

    /**
     * ユーザーの監査ログを取得
     *
     * @param userId ユーザーID
     * @param startDate 開始日時
     * @param endDate 終了日時
     * @return 監査ログリスト
     */
    public List<AuditLog> getLogsByUser(String userId, LocalDateTime startDate, LocalDateTime endDate) {
        return auditLogRepository.findByUser(userId, startDate, endDate);
    }

    /**
     * 期間で監査ログを取得
     *
     * @param startDate 開始日時
     * @param endDate 終了日時
     * @param limit 取得件数上限
     * @return 監査ログリスト
     */
    public List<AuditLog> getLogsByPeriod(LocalDateTime startDate, LocalDateTime endDate, int limit) {
        return auditLogRepository.findByPeriod(startDate, endDate, limit);
    }

    /**
     * 全ての監査ログを取得
     *
     * @return 監査ログリスト
     */
    public List<AuditLog> getAllLogs() {
        return auditLogRepository.findAll();
    }
}
