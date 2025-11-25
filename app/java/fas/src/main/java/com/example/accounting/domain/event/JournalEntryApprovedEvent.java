package com.example.accounting.domain.event;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.Builder;
import lombok.Value;
import lombok.extern.jackson.Jacksonized;

import java.time.LocalDateTime;

/**
 * 仕訳承認イベント（イベントソーシング用）
 */
@Value
@Builder
@Jacksonized
@JsonIgnoreProperties(ignoreUnknown = true)
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
