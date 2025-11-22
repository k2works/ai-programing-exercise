package com.example.accounting.domain;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;

/**
 * 仕訳ドメインエンティティ
 */
public class Journal {
    private Integer journalId;
    private LocalDate journalDate;
    private String description;
    private Integer fiscalYear;
    private List<JournalEntry> entries;

    public Journal(LocalDate journalDate, String description, Integer fiscalYear) {
        this.journalDate = journalDate;
        this.description = description;
        this.fiscalYear = fiscalYear;
        this.entries = new ArrayList<>();
    }

    /**
     * 仕訳明細を追加（ビジネスルール: 借方・貸方の検証）
     */
    public void addEntry(JournalEntry entry) {
        validateEntry(entry);
        this.entries.add(entry);
    }

    /**
     * 借方合計と貸方合計が一致することを検証
     */
    public void validateBalance() {
        BigDecimal debitTotal = entries.stream()
            .map(JournalEntry::getDebitAmount)
            .reduce(BigDecimal.ZERO, BigDecimal::add);

        BigDecimal creditTotal = entries.stream()
            .map(JournalEntry::getCreditAmount)
            .reduce(BigDecimal.ZERO, BigDecimal::add);

        if (debitTotal.compareTo(creditTotal) != 0) {
            throw new IllegalStateException(
                String.format("貸借が一致しません。借方合計: %s, 貸方合計: %s", debitTotal, creditTotal)
            );
        }
    }

    private void validateEntry(JournalEntry entry) {
        if (entry.getDebitAmount().compareTo(BigDecimal.ZERO) == 0 &&
            entry.getCreditAmount().compareTo(BigDecimal.ZERO) == 0) {
            throw new IllegalArgumentException("借方・貸方の少なくとも一方は0より大きい必要があります");
        }
    }

    // Getters, setters
    public Integer getJournalId() {
        return journalId;
    }

    public void setJournalId(Integer journalId) {
        this.journalId = journalId;
    }

    public LocalDate getJournalDate() {
        return journalDate;
    }

    public String getDescription() {
        return description;
    }

    public Integer getFiscalYear() {
        return fiscalYear;
    }

    public List<JournalEntry> getEntries() {
        return entries;
    }
}
