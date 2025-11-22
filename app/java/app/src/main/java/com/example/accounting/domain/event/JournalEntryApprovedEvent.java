package com.example.accounting.domain.event;

import lombok.Builder;
import lombok.Value;

import java.time.LocalDateTime;

/**
 * 仕訳承認イベント（イベントソーシング用）
 */
@Value
@Builder
public class JournalEntryApprovedEvent extends DomainEvent {
    String journalEntryId;
    String approvedBy;
    String approvalComment;
    LocalDateTime occurredAt;
    String userId;

    @Override
    public String getEventType() {
        return "JournalEntryApprovedEvent";
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
