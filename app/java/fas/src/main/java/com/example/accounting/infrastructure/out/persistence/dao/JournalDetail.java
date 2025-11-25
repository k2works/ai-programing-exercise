package com.example.accounting.infrastructure.out.persistence.dao;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

/**
 * 仕訳明細エンティティ
 */
public class JournalDetail {
    private String journalNo;              // 仕訳伝票番号
    private Integer lineNumber;            // 仕訳行番号
    private String description;            // 行摘要
    private LocalDateTime createdAt;       // 作成日時
    private LocalDateTime updatedAt;       // 更新日時
    private List<JournalDetailItem> items; // 仕訳貸借明細リスト

    public JournalDetail() {
        this.items = new ArrayList<>();
    }

    public JournalDetail(String journalNo, Integer lineNumber, String description) {
        this.journalNo = journalNo;
        this.lineNumber = lineNumber;
        this.description = description;
        this.items = new ArrayList<>();
    }

    // Getters
    public String getJournalNo() {
        return journalNo;
    }

    public Integer getLineNumber() {
        return lineNumber;
    }

    public String getDescription() {
        return description;
    }

    public LocalDateTime getCreatedAt() {
        return createdAt;
    }

    public LocalDateTime getUpdatedAt() {
        return updatedAt;
    }

    public List<JournalDetailItem> getItems() {
        return items;
    }

    // Setters
    public void setJournalNo(String journalNo) {
        this.journalNo = journalNo;
    }

    public void setLineNumber(Integer lineNumber) {
        this.lineNumber = lineNumber;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public void setCreatedAt(LocalDateTime createdAt) {
        this.createdAt = createdAt;
    }

    public void setUpdatedAt(LocalDateTime updatedAt) {
        this.updatedAt = updatedAt;
    }

    public void setItems(List<JournalDetailItem> items) {
        this.items = items;
    }

    // Business methods
    public void addItem(JournalDetailItem item) {
        this.items.add(item);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        JournalDetail that = (JournalDetail) o;
        return Objects.equals(journalNo, that.journalNo)
                && Objects.equals(lineNumber, that.lineNumber);
    }

    @Override
    public int hashCode() {
        return Objects.hash(journalNo, lineNumber);
    }

    @Override
    public String toString() {
        return "JournalDetail{"
                + "journalNo='" + journalNo + '\''
                + ", lineNumber=" + lineNumber
                + ", description='" + description + '\''
                + ", createdAt=" + createdAt
                + ", updatedAt=" + updatedAt
                + ", items=" + items
                + '}';
    }
}
