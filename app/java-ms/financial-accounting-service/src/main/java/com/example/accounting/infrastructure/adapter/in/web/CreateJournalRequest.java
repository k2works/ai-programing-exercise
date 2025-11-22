package com.example.accounting.infrastructure.adapter.in.web;

import com.example.accounting.port.in.JournalEntryRequest;
import java.time.LocalDate;
import java.util.List;

public class CreateJournalRequest {
    private LocalDate journalDate;
    private String description;
    private Integer fiscalYear;
    private List<JournalEntryRequest> entries;

    public CreateJournalRequest() {
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

    public List<JournalEntryRequest> getEntries() {
        return entries;
    }

    public void setEntries(List<JournalEntryRequest> entries) {
        this.entries = entries;
    }
}
