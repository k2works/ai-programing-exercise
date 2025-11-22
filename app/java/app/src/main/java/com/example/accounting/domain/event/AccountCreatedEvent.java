package com.example.accounting.domain.event;

import lombok.Getter;

import java.util.Map;

/**
 * 勘定科目作成イベント
 */
@Getter
public class AccountCreatedEvent extends DomainEvent {
    private final String accountCode;
    private final Map<String, Object> accountData;
    private final String userId;
    private final String userName;
    private final String ipAddress;

    public AccountCreatedEvent(
            String accountCode,
            Map<String, Object> accountData,
            String userId,
            String userName,
            String ipAddress) {
        super();
        this.accountCode = accountCode;
        this.accountData = accountData;
        this.userId = userId;
        this.userName = userName;
        this.ipAddress = ipAddress;
    }

    @Override
    public String getEventType() {
        return "AccountCreated";
    }
}
