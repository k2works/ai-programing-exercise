package com.example.accounting.infrastructure.in.web.dto;

import com.example.accounting.domain.model.audit.AuditLog;
import com.fasterxml.jackson.annotation.JsonFormat;
import lombok.Builder;
import lombok.Data;

import java.time.LocalDateTime;
import java.util.Map;

/**
 * 監査ログレスポンス DTO
 */
@Data
@Builder
public class AuditLogResponse {
    private Long id;
    private String entityType;
    private String entityId;
    private String action;
    private String userId;
    private String userName;

    @JsonFormat(pattern = "yyyy-MM-dd'T'HH:mm:ss")
    private LocalDateTime timestamp;

    private Map<String, Object> oldValues;
    private Map<String, Object> newValues;
    private Map<String, Object> changes;
    private String reason;
    private String summary;

    /**
     * ドメインモデルから DTO に変換
     *
     * @param auditLog 監査ログ
     * @return DTO
     */
    public static AuditLogResponse from(AuditLog auditLog) {
        return AuditLogResponse.builder()
                .id(auditLog.getId())
                .entityType(auditLog.getEntityType())
                .entityId(auditLog.getEntityId())
                .action(auditLog.getAction().name())
                .userId(auditLog.getUserId())
                .userName(auditLog.getUserName())
                .timestamp(auditLog.getTimestamp())
                .oldValues(auditLog.getOldValues())
                .newValues(auditLog.getNewValues())
                .changes(auditLog.getChanges())
                .reason(auditLog.getReason())
                .summary(auditLog.getSummary())
                .build();
    }
}
