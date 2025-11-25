package com.example.accounting.infrastructure.out.persistence.dao;

import lombok.Data;

import java.time.LocalDateTime;

/**
 * 監査ログDAO
 * （データベーステーブルとのマッピング用エンティティ）
 */
@Data
public class AuditLog {
    private Long logId;
    private String entityType;
    private String entityId;
    private String action;
    private String userId;
    private String userName;
    private LocalDateTime timestamp;
    private String oldValues;      // JSONB → String
    private String newValues;      // JSONB → String
    private String changes;        // JSONB → String
    private String reason;
    private String ipAddress;
    private String userAgent;
    private LocalDateTime createdAt;
}
