package com.example.accounting.domain.event;

import lombok.Getter;

import java.util.Map;

/**
 * 勘定科目更新イベント
 */
@Getter
public class AccountUpdatedEvent extends DomainEvent {
    private final String accountCode;
    private final Map<String, Object> oldValues;
    private final Map<String, Object> newValues;
    private final String userId;
    private final String userName;
    private final String ipAddress;

    public AccountUpdatedEvent(
            String accountCode,
            Map<String, Object> oldValues,
            Map<String, Object> newValues,
            String userId,
            String userName,
            String ipAddress) {
        super();
        this.accountCode = accountCode;
        this.oldValues = oldValues;
        this.newValues = newValues;
        this.userId = userId;
        this.userName = userName;
        this.ipAddress = ipAddress;
    }

    @Override
    public String getEventType() {
        return "AccountUpdated";
    }

    @Override
    public String getAggregateId() {
        return accountCode;
    }

    @Override
    public String getUserId() {
        return userId;
    }
}
