package com.example.accounting.domain.event;

import lombok.Builder;
import lombok.Value;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;

/**
 * 仕訳作成イベント（イベントソーシング用）
 */
@Value
@Builder
public class JournalEntryCreatedEvent extends DomainEvent {
    String journalEntryId;
    LocalDate entryDate;
    String description;
    List<JournalEntryLineItem> lineItems;
    String userId;
    LocalDateTime occurredAt;

    @Override
    public String getEventType() {
        return "JournalEntryCreatedEvent";
    }

    @Override
    public String getAggregateId() {
        return journalEntryId;
    }

    @Override
    public String getUserId() {
        return userId;
    }

    /**
     * 仕訳明細行
     */
    @Value
    @Builder
    public static class JournalEntryLineItem {
        String accountCode;
        DebitCredit debitCredit;
        BigDecimal amount;
    }

    /**
     * 借方・貸方区分
     */
    public enum DebitCredit {
        DEBIT, CREDIT
    }
}
