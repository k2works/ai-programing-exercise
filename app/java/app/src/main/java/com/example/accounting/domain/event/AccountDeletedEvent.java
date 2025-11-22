package com.example.accounting.domain.event;

import lombok.Getter;

import java.util.Map;

/**
 * 勘定科目削除イベント
 */
@Getter
public class AccountDeletedEvent extends DomainEvent {
    private final String accountCode;
    private final Map<String, Object> oldValues;
    private final String reason;
    private final String userId;
    private final String userName;
    private final String ipAddress;

    public AccountDeletedEvent(
            String accountCode,
            Map<String, Object> oldValues,
            String reason,
            String userId,
            String userName,
            String ipAddress) {
        super();
        this.accountCode = accountCode;
        this.oldValues = oldValues;
        this.reason = reason;
        this.userId = userId;
        this.userName = userName;
        this.ipAddress = ipAddress;
    }

    @Override
    public String getEventType() {
        return "AccountDeleted";
    }
}
