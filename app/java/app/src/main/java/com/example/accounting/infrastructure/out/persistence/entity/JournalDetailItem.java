package com.example.accounting.infrastructure.out.persistence.entity;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Objects;

/**
 * 仕訳貸借明細エンティティ
 */
public class JournalDetailItem {
    private String journalNo;            // 仕訳伝票番号
    private Integer lineNumber;          // 仕訳行番号
    private String debitCreditFlag;      // 仕訳行貸借区分（D=借方、C=貸方）
    private String currencyCode;         // 通貨コード
    private BigDecimal exchangeRate;     // 為替レート
    private String departmentCode;       // 部門コード
    private String projectCode;          // プロジェクトコード
    private String accountCode;          // 勘定科目コード
    private String subAccountCode;       // 補助科目コード
    private BigDecimal amount;           // 仕訳金額
    private BigDecimal baseAmount;       // 基軸換算仕訳金額
    private String taxType;              // 消費税区分
    private Integer taxRate;             // 消費税率
    private String taxCalcType;          // 消費税計算区分
    private LocalDate dueDate;           // 期日
    private Integer cashFlowFlag;        // 資金繰フラグ
    private String segmentCode;          // セグメントコード
    private String offsetAccountCode;    // 相手勘定科目コード
    private String offsetSubAccountCode; // 相手補助科目コード
    private String noteCode;             // 付箋コード
    private String noteContent;          // 付箋内容
    private LocalDateTime createdAt;     // 作成日時
    private LocalDateTime updatedAt;     // 更新日時

    public JournalDetailItem() {
    }

    public JournalDetailItem(String journalNo, Integer lineNumber, String debitCreditFlag,
                             String accountCode, BigDecimal amount) {
        this.journalNo = journalNo;
        this.lineNumber = lineNumber;
        this.debitCreditFlag = debitCreditFlag;
        this.accountCode = accountCode;
        this.amount = amount;
        this.baseAmount = amount;
        this.currencyCode = "JPY";
        this.exchangeRate = BigDecimal.ONE;
        this.cashFlowFlag = 0;
    }

    // Getters
    public String getJournalNo() {
        return journalNo;
    }

    public Integer getLineNumber() {
        return lineNumber;
    }

    public String getDebitCreditFlag() {
        return debitCreditFlag;
    }

    public String getCurrencyCode() {
        return currencyCode;
    }

    public BigDecimal getExchangeRate() {
        return exchangeRate;
    }

    public String getDepartmentCode() {
        return departmentCode;
    }

    public String getProjectCode() {
        return projectCode;
    }

    public String getAccountCode() {
        return accountCode;
    }

    public String getSubAccountCode() {
        return subAccountCode;
    }

    public BigDecimal getAmount() {
        return amount;
    }

    public BigDecimal getBaseAmount() {
        return baseAmount;
    }

    public String getTaxType() {
        return taxType;
    }

    public Integer getTaxRate() {
        return taxRate;
    }

    public String getTaxCalcType() {
        return taxCalcType;
    }

    public LocalDate getDueDate() {
        return dueDate;
    }

    public Integer getCashFlowFlag() {
        return cashFlowFlag;
    }

    public String getSegmentCode() {
        return segmentCode;
    }

    public String getOffsetAccountCode() {
        return offsetAccountCode;
    }

    public String getOffsetSubAccountCode() {
        return offsetSubAccountCode;
    }

    public String getNoteCode() {
        return noteCode;
    }

    public String getNoteContent() {
        return noteContent;
    }

    public LocalDateTime getCreatedAt() {
        return createdAt;
    }

    public LocalDateTime getUpdatedAt() {
        return updatedAt;
    }

    // Setters
    public void setJournalNo(String journalNo) {
        this.journalNo = journalNo;
    }

    public void setLineNumber(Integer lineNumber) {
        this.lineNumber = lineNumber;
    }

    public void setDebitCreditFlag(String debitCreditFlag) {
        this.debitCreditFlag = debitCreditFlag;
    }

    public void setCurrencyCode(String currencyCode) {
        this.currencyCode = currencyCode;
    }

    public void setExchangeRate(BigDecimal exchangeRate) {
        this.exchangeRate = exchangeRate;
    }

    public void setDepartmentCode(String departmentCode) {
        this.departmentCode = departmentCode;
    }

    public void setProjectCode(String projectCode) {
        this.projectCode = projectCode;
    }

    public void setAccountCode(String accountCode) {
        this.accountCode = accountCode;
    }

    public void setSubAccountCode(String subAccountCode) {
        this.subAccountCode = subAccountCode;
    }

    public void setAmount(BigDecimal amount) {
        this.amount = amount;
    }

    public void setBaseAmount(BigDecimal baseAmount) {
        this.baseAmount = baseAmount;
    }

    public void setTaxType(String taxType) {
        this.taxType = taxType;
    }

    public void setTaxRate(Integer taxRate) {
        this.taxRate = taxRate;
    }

    public void setTaxCalcType(String taxCalcType) {
        this.taxCalcType = taxCalcType;
    }

    public void setDueDate(LocalDate dueDate) {
        this.dueDate = dueDate;
    }

    public void setCashFlowFlag(Integer cashFlowFlag) {
        this.cashFlowFlag = cashFlowFlag;
    }

    public void setSegmentCode(String segmentCode) {
        this.segmentCode = segmentCode;
    }

    public void setOffsetAccountCode(String offsetAccountCode) {
        this.offsetAccountCode = offsetAccountCode;
    }

    public void setOffsetSubAccountCode(String offsetSubAccountCode) {
        this.offsetSubAccountCode = offsetSubAccountCode;
    }

    public void setNoteCode(String noteCode) {
        this.noteCode = noteCode;
    }

    public void setNoteContent(String noteContent) {
        this.noteContent = noteContent;
    }

    public void setCreatedAt(LocalDateTime createdAt) {
        this.createdAt = createdAt;
    }

    public void setUpdatedAt(LocalDateTime updatedAt) {
        this.updatedAt = updatedAt;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        JournalDetailItem that = (JournalDetailItem) o;
        return Objects.equals(journalNo, that.journalNo)
                && Objects.equals(lineNumber, that.lineNumber)
                && Objects.equals(debitCreditFlag, that.debitCreditFlag);
    }

    @Override
    public int hashCode() {
        return Objects.hash(journalNo, lineNumber, debitCreditFlag);
    }

    @Override
    public String toString() {
        return "JournalDetailItem{"
                + "journalNo='" + journalNo + '\''
                + ", lineNumber=" + lineNumber
                + ", debitCreditFlag='" + debitCreditFlag + '\''
                + ", currencyCode='" + currencyCode + '\''
                + ", exchangeRate=" + exchangeRate
                + ", departmentCode='" + departmentCode + '\''
                + ", projectCode='" + projectCode + '\''
                + ", accountCode='" + accountCode + '\''
                + ", subAccountCode='" + subAccountCode + '\''
                + ", amount=" + amount
                + ", baseAmount=" + baseAmount
                + ", taxType='" + taxType + '\''
                + ", taxRate=" + taxRate
                + ", taxCalcType='" + taxCalcType + '\''
                + ", dueDate=" + dueDate
                + ", cashFlowFlag=" + cashFlowFlag
                + ", segmentCode='" + segmentCode + '\''
                + ", offsetAccountCode='" + offsetAccountCode + '\''
                + ", offsetSubAccountCode='" + offsetSubAccountCode + '\''
                + ", noteCode='" + noteCode + '\''
                + ", noteContent='" + noteContent + '\''
                + ", createdAt=" + createdAt
                + ", updatedAt=" + updatedAt
                + '}';
    }
}
