package com.example.accounting.domain;

import java.math.BigDecimal;

/**
 * 仕訳明細ドメインエンティティ
 */
public class JournalEntry {
    private Integer journalEntryId;
    private Integer journalId;
    private String accountCode;
    private BigDecimal debitAmount;
    private BigDecimal creditAmount;
    private String description;

    public JournalEntry(String accountCode, BigDecimal debitAmount, BigDecimal creditAmount, String description) {
        this.accountCode = accountCode;
        this.debitAmount = debitAmount != null ? debitAmount : BigDecimal.ZERO;
        this.creditAmount = creditAmount != null ? creditAmount : BigDecimal.ZERO;
        this.description = description;
    }

    // Getters
    public Integer getJournalEntryId() {
        return journalEntryId;
    }

    public void setJournalEntryId(Integer journalEntryId) {
        this.journalEntryId = journalEntryId;
    }

    public Integer getJournalId() {
        return journalId;
    }

    public void setJournalId(Integer journalId) {
        this.journalId = journalId;
    }

    public String getAccountCode() {
        return accountCode;
    }

    public BigDecimal getDebitAmount() {
        return debitAmount;
    }

    public BigDecimal getCreditAmount() {
        return creditAmount;
    }

    public String getDescription() {
        return description;
    }
}
