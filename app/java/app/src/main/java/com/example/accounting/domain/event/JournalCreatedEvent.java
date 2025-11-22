package com.example.accounting.domain.event;

import lombok.Getter;

import java.util.Map;

/**
 * 仕訳作成イベント
 */
@Getter
public class JournalCreatedEvent extends DomainEvent {
    private final String journalNo;
    private final Map<String, Object> journalData;
    private final String userId;
    private final String userName;
    private final String ipAddress;

    public JournalCreatedEvent(
            String journalNo,
            Map<String, Object> journalData,
            String userId,
            String userName,
            String ipAddress) {
        super();
        this.journalNo = journalNo;
        this.journalData = journalData;
        this.userId = userId;
        this.userName = userName;
        this.ipAddress = ipAddress;
    }

    @Override
    public String getEventType() {
        return "JournalCreated";
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
