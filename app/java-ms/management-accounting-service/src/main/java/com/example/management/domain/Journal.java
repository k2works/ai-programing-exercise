package com.example.management.domain;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;

/**
 * 財務会計サービスから取得する仕訳データ (DTO)
 */
public class Journal {
    private Integer journalId;
    private LocalDate journalDate;
    private String description;
    private Integer fiscalYear;
    private List<JournalEntry> entries;

    // Default constructor for JSON deserialization
    public Journal() {
    }

    // Getters and setters
    public Integer getJournalId() {
        return journalId;
    }

    public void setJournalId(Integer journalId) {
        this.journalId = journalId;
    }

    public LocalDate getJournalDate() {
        return journalDate;
    }

    public void setJournalDate(LocalDate journalDate) {
        this.journalDate = journalDate;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public Integer getFiscalYear() {
        return fiscalYear;
    }

    public void setFiscalYear(Integer fiscalYear) {
        this.fiscalYear = fiscalYear;
    }

    public List<JournalEntry> getEntries() {
        return entries;
    }

    public void setEntries(List<JournalEntry> entries) {
        this.entries = entries;
    }

    public static class JournalEntry {
        private String accountCode;
        private BigDecimal debitAmount;
        private BigDecimal creditAmount;

        // Getters and setters
        public String getAccountCode() {
            return accountCode;
        }

        public void setAccountCode(String accountCode) {
            this.accountCode = accountCode;
        }

        public BigDecimal getDebitAmount() {
            return debitAmount;
        }

        public void setDebitAmount(BigDecimal debitAmount) {
            this.debitAmount = debitAmount;
        }

        public BigDecimal getCreditAmount() {
            return creditAmount;
        }

        public void setCreditAmount(BigDecimal creditAmount) {
            this.creditAmount = creditAmount;
        }
    }
}
