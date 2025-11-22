package com.example.accounting.infrastructure.out.persistence.entity;

import java.time.LocalDateTime;
import java.util.Objects;

/**
 * 自動仕訳管理エンティティ
 *
 * 各ソーステーブルの最終処理日時を管理し、差分処理を実現する
 */
public class AutoJournalManagement {

    private Long id;                        // ID
    private String sourceTableName;         // ソーステーブル名
    private LocalDateTime lastProcessedAt;  // 最終処理日時
    private LocalDateTime createdAt;        // 作成日時
    private LocalDateTime updatedAt;        // 更新日時

    /**
     * デフォルトコンストラクタ
     */
    public AutoJournalManagement() {
    }

    /**
     * 全フィールドコンストラクタ
     */
    public AutoJournalManagement(Long id, String sourceTableName, LocalDateTime lastProcessedAt,
                                 LocalDateTime createdAt, LocalDateTime updatedAt) {
        this.id = id;
        this.sourceTableName = sourceTableName;
        this.lastProcessedAt = lastProcessedAt;
        this.createdAt = createdAt;
        this.updatedAt = updatedAt;
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getSourceTableName() {
        return sourceTableName;
    }

    public void setSourceTableName(String sourceTableName) {
        this.sourceTableName = sourceTableName;
    }

    public LocalDateTime getLastProcessedAt() {
        return lastProcessedAt;
    }

    public void setLastProcessedAt(LocalDateTime lastProcessedAt) {
        this.lastProcessedAt = lastProcessedAt;
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

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        AutoJournalManagement that = (AutoJournalManagement) o;
        return Objects.equals(id, that.id)
                && Objects.equals(sourceTableName, that.sourceTableName)
                && Objects.equals(lastProcessedAt, that.lastProcessedAt)
                && Objects.equals(createdAt, that.createdAt)
                && Objects.equals(updatedAt, that.updatedAt);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id, sourceTableName, lastProcessedAt, createdAt, updatedAt);
    }

    @Override
    public String toString() {
        return "AutoJournalManagement{"
                + "id=" + id
                + ", sourceTableName='" + sourceTableName + '\''
                + ", lastProcessedAt=" + lastProcessedAt
                + ", createdAt=" + createdAt
                + ", updatedAt=" + updatedAt
                + '}';
    }
}
