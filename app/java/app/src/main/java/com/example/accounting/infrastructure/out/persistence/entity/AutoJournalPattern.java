package com.example.accounting.infrastructure.out.persistence.entity;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

/**
 * 自動仕訳パターンエンティティ
 *
 * 仕訳生成ルールを定義する
 */
public class AutoJournalPattern {

    private Long id;                            // ID
    private String patternCode;                 // パターンコード
    private String patternName;                 // パターン名
    private String sourceTableName;             // ソーステーブル名
    private String description;                 // 説明
    private Boolean isActive;                   // 有効フラグ
    private LocalDateTime createdAt;            // 作成日時
    private LocalDateTime updatedAt;            // 更新日時
    private List<AutoJournalPatternItem> items; // パターン明細リスト

    /**
     * デフォルトコンストラクタ
     */
    public AutoJournalPattern() {
        this.items = new ArrayList<>();
    }

    /**
     * 全フィールドコンストラクタ
     */
    @SuppressWarnings("checkstyle:ParameterNumber")
    public AutoJournalPattern(Long id, String patternCode, String patternName, String sourceTableName,
                              String description, Boolean isActive, LocalDateTime createdAt,
                              LocalDateTime updatedAt, List<AutoJournalPatternItem> items) {
        this.id = id;
        this.patternCode = patternCode;
        this.patternName = patternName;
        this.sourceTableName = sourceTableName;
        this.description = description;
        this.isActive = isActive;
        this.createdAt = createdAt;
        this.updatedAt = updatedAt;
        this.items = items != null ? items : new ArrayList<>();
    }

    /**
     * パターン明細を追加
     */
    public void addItem(AutoJournalPatternItem item) {
        this.items.add(item);
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getPatternCode() {
        return patternCode;
    }

    public void setPatternCode(String patternCode) {
        this.patternCode = patternCode;
    }

    public String getPatternName() {
        return patternName;
    }

    public void setPatternName(String patternName) {
        this.patternName = patternName;
    }

    public String getSourceTableName() {
        return sourceTableName;
    }

    public void setSourceTableName(String sourceTableName) {
        this.sourceTableName = sourceTableName;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public Boolean getIsActive() {
        return isActive;
    }

    public void setIsActive(Boolean isActive) {
        this.isActive = isActive;
    }

    public LocalDateTime getCreatedAt() {
        return createdAt;
    }

    public void setCreatedAt(LocalDateTime createdAt) {
        this.createdAt = createdAt;
    }

    public LocalDateTime getUpdatedAt() {
        return updatedAt;
    }

    public void setUpdatedAt(LocalDateTime updatedAt) {
        this.updatedAt = updatedAt;
    }

    public List<AutoJournalPatternItem> getItems() {
        return items;
    }

    public void setItems(List<AutoJournalPatternItem> items) {
        this.items = items;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        AutoJournalPattern that = (AutoJournalPattern) o;
        return Objects.equals(id, that.id)
                && Objects.equals(patternCode, that.patternCode)
                && Objects.equals(patternName, that.patternName)
                && Objects.equals(sourceTableName, that.sourceTableName)
                && Objects.equals(description, that.description)
                && Objects.equals(isActive, that.isActive)
                && Objects.equals(createdAt, that.createdAt)
                && Objects.equals(updatedAt, that.updatedAt)
                && Objects.equals(items, that.items);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id, patternCode, patternName, sourceTableName, description,
                isActive, createdAt, updatedAt, items);
    }

    @Override
    public String toString() {
        return "AutoJournalPattern{"
                + "id=" + id
                + ", patternCode='" + patternCode + '\''
                + ", patternName='" + patternName + '\''
                + ", sourceTableName='" + sourceTableName + '\''
                + ", description='" + description + '\''
                + ", isActive=" + isActive
                + ", createdAt=" + createdAt
                + ", updatedAt=" + updatedAt
                + ", items=" + items
                + '}';
    }
}
