package com.example.accounting.domain.event;

import lombok.Getter;

import java.util.Map;

/**
 * 仕訳更新イベント
 */
@Getter
public class JournalUpdatedEvent extends DomainEvent {
    private final String journalNo;
    private final Map<String, Object> oldValues;
    private final Map<String, Object> newValues;
    private final String userId;
    private final String userName;
    private final String ipAddress;

    public JournalUpdatedEvent(
            String journalNo,
            Map<String, Object> oldValues,
            Map<String, Object> newValues,
            String userId,
            String userName,
            String ipAddress) {
        super();
        this.journalNo = journalNo;
        this.oldValues = oldValues;
        this.newValues = newValues;
        this.userId = userId;
        this.userName = userName;
        this.ipAddress = ipAddress;
    }

    @Override
    public String getEventType() {
        return "JournalUpdated";
    }

    @Override
    public String getAggregateId() {
        return journalNo;
    }

    @Override
    public String getUserId() {
        return userId;
    }
}
