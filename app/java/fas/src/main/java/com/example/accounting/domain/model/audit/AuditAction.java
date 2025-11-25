package com.example.accounting.domain.model.audit;

/**
 * 監査操作種別
 */
public enum AuditAction {
    CREATE("作成"),
    UPDATE("更新"),
    DELETE("削除");

    private final String displayName;

    AuditAction(String displayName) {
        this.displayName = displayName;
    }

    public String getDisplayName() {
        return displayName;
    }
}
