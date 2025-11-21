package com.example.accounting.infrastructure.persistence.entity;

import java.time.LocalDateTime;
import java.util.Objects;

/**
 * 自動仕訳パターン明細エンティティ
 *
 * パターンの詳細定義（勘定科目、金額式、摘要テンプレート等）
 */
public class AutoJournalPatternItem {

    private Long id;                        // ID
    private Long patternId;                 // パターンID
    private Integer lineNumber;             // 行番号
    private String debitCreditFlag;         // 貸借区分 (D=借方、C=貸方)
    private String accountCode;             // 勘定科目コード
    private String amountExpression;        // 金額式
    private String descriptionTemplate;     // 摘要テンプレート
    private LocalDateTime createdAt;        // 作成日時
    private LocalDateTime updatedAt;        // 更新日時

    /**
     * デフォルトコンストラクタ
     */
    public AutoJournalPatternItem() {
    }

    /**
     * 全フィールドコンストラクタ
     */
    @SuppressWarnings("checkstyle:ParameterNumber")
    public AutoJournalPatternItem(Long id, Long patternId, Integer lineNumber, String debitCreditFlag,
                                  String accountCode, String amountExpression, String descriptionTemplate,
                                  LocalDateTime createdAt, LocalDateTime updatedAt) {
        this.id = id;
        this.patternId = patternId;
        this.lineNumber = lineNumber;
        this.debitCreditFlag = debitCreditFlag;
        this.accountCode = accountCode;
        this.amountExpression = amountExpression;
        this.descriptionTemplate = descriptionTemplate;
        this.createdAt = createdAt;
        this.updatedAt = updatedAt;
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

    public Integer getLineNumber() {
        return lineNumber;
    }

    public void setLineNumber(Integer lineNumber) {
        this.lineNumber = lineNumber;
    }

    public String getDebitCreditFlag() {
        return debitCreditFlag;
    }

    public void setDebitCreditFlag(String debitCreditFlag) {
        this.debitCreditFlag = debitCreditFlag;
    }

    public String getAccountCode() {
        return accountCode;
    }

    public void setAccountCode(String accountCode) {
        this.accountCode = accountCode;
    }

    public String getAmountExpression() {
        return amountExpression;
    }

    public void setAmountExpression(String amountExpression) {
        this.amountExpression = amountExpression;
    }

    public String getDescriptionTemplate() {
        return descriptionTemplate;
    }

    public void setDescriptionTemplate(String descriptionTemplate) {
        this.descriptionTemplate = descriptionTemplate;
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
        AutoJournalPatternItem that = (AutoJournalPatternItem) o;
        return Objects.equals(id, that.id)
                && Objects.equals(patternId, that.patternId)
                && Objects.equals(lineNumber, that.lineNumber)
                && Objects.equals(debitCreditFlag, that.debitCreditFlag)
                && Objects.equals(accountCode, that.accountCode)
                && Objects.equals(amountExpression, that.amountExpression)
                && Objects.equals(descriptionTemplate, that.descriptionTemplate)
                && Objects.equals(createdAt, that.createdAt)
                && Objects.equals(updatedAt, that.updatedAt);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id, patternId, lineNumber, debitCreditFlag, accountCode,
                amountExpression, descriptionTemplate, createdAt, updatedAt);
    }

    @Override
    public String toString() {
        return "AutoJournalPatternItem{"
                + "id=" + id
                + ", patternId=" + patternId
                + ", lineNumber=" + lineNumber
                + ", debitCreditFlag='" + debitCreditFlag + '\''
                + ", accountCode='" + accountCode + '\''
                + ", amountExpression='" + amountExpression + '\''
                + ", descriptionTemplate='" + descriptionTemplate + '\''
                + ", createdAt=" + createdAt
                + ", updatedAt=" + updatedAt
                + '}';
    }
}
