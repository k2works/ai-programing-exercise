package com.example.accounting.domain.event;

import lombok.Builder;
import lombok.Value;

import java.time.LocalDateTime;

/**
 * 仕訳削除イベント（イベントソーシング用）
 */
@Value
@Builder
public class JournalEntryDeletedEvent extends DomainEvent {
    String journalEntryId;
    String reason;
    LocalDateTime occurredAt;
    String userId;

    @Override
    public String getEventType() {
        return "JournalEntryDeletedEvent";
    }

    @Override
    public String getAggregateId() {
        return journalEntryId;
    }

    @Override
    public String getUserId() {
        return userId;
    }
}
