package com.example.accounting.domain.event;

import lombok.Getter;

import java.util.Map;

/**
 * 仕訳削除イベント
 */
@Getter
public class JournalDeletedEvent extends DomainEvent {
    private final String journalNo;
    private final Map<String, Object> oldValues;
    private final String reason;
    private final String userId;
    private final String userName;
    private final String ipAddress;

    public JournalDeletedEvent(
            String journalNo,
            Map<String, Object> oldValues,
            String reason,
            String userId,
            String userName,
            String ipAddress) {
        super();
        this.journalNo = journalNo;
        this.oldValues = oldValues;
        this.reason = reason;
        this.userId = userId;
        this.userName = userName;
        this.ipAddress = ipAddress;
    }

    @Override
    public String getEventType() {
        return "JournalDeleted";
    }
}
