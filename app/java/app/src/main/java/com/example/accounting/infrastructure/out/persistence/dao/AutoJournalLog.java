package com.example.accounting.infrastructure.out.persistence.dao;

import java.time.LocalDateTime;
import java.util.Objects;

/**
 * 自動仕訳実行ログエンティティ
 *
 * 実行履歴と監査証跡を記録する
 */
public class AutoJournalLog {

    private Long id;                    // ID
    private Long patternId;             // パターンID
    private LocalDateTime executedAt;   // 実行日時
    private Integer processedCount;     // 処理件数
    private Integer generatedCount;     // 生成件数
    private String status;              // ステータス (SUCCESS, FAILURE, RUNNING)
    private String message;             // メッセージ
    private String errorDetail;         // エラー詳細
    private LocalDateTime createdAt;    // 作成日時

    /**
     * デフォルトコンストラクタ
     */
    public AutoJournalLog() {
    }

    /**
     * 全フィールドコンストラクタ
     */
    @SuppressWarnings("checkstyle:ParameterNumber")
    public AutoJournalLog(Long id, Long patternId, LocalDateTime executedAt, Integer processedCount,
                          Integer generatedCount, String status, String message, String errorDetail,
                          LocalDateTime createdAt) {
        this.id = id;
        this.patternId = patternId;
        this.executedAt = executedAt;
        this.processedCount = processedCount;
        this.generatedCount = generatedCount;
        this.status = status;
        this.message = message;
        this.errorDetail = errorDetail;
        this.createdAt = createdAt;
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Long getPatternId() {
        return patternId;
    }

    public void setPatternId(Long patternId) {
        this.patternId = patternId;
    }

    public LocalDateTime getExecutedAt() {
        return executedAt;
    }

    public void setExecutedAt(LocalDateTime executedAt) {
        this.executedAt = executedAt;
    }

    public Integer getProcessedCount() {
        return processedCount;
    }

    public void setProcessedCount(Integer processedCount) {
        this.processedCount = processedCount;
    }

    public Integer getGeneratedCount() {
        return generatedCount;
    }

    public void setGeneratedCount(Integer generatedCount) {
        this.generatedCount = generatedCount;
    }

    public String getStatus() {
        return status;
    }

    public void setStatus(String status) {
        this.status = status;
    }

    public String getMessage() {
        return message;
    }

    public void setMessage(String message) {
        this.message = message;
    }

    public String getErrorDetail() {
        return errorDetail;
    }

    public void setErrorDetail(String errorDetail) {
        this.errorDetail = errorDetail;
    }

    public LocalDateTime getCreatedAt() {
        return createdAt;
    }

    public void setCreatedAt(LocalDateTime createdAt) {
        this.createdAt = createdAt;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        AutoJournalLog that = (AutoJournalLog) o;
        return Objects.equals(id, that.id)
                && Objects.equals(patternId, that.patternId)
                && Objects.equals(executedAt, that.executedAt)
                && Objects.equals(processedCount, that.processedCount)
                && Objects.equals(generatedCount, that.generatedCount)
                && Objects.equals(status, that.status)
                && Objects.equals(message, that.message)
                && Objects.equals(errorDetail, that.errorDetail)
                && Objects.equals(createdAt, that.createdAt);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id, patternId, executedAt, processedCount, generatedCount,
                status, message, errorDetail, createdAt);
    }

    @Override
    public String toString() {
        return "AutoJournalLog{"
                + "id=" + id
                + ", patternId=" + patternId
                + ", executedAt=" + executedAt
                + ", processedCount=" + processedCount
                + ", generatedCount=" + generatedCount
                + ", status='" + status + '\''
                + ", message='" + message + '\''
                + ", errorDetail='" + errorDetail + '\''
                + ", createdAt=" + createdAt
                + '}';
    }
}
