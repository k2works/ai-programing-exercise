package com.example.accounting.infrastructure.out.persistence.entity;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

/**
 * 仕訳エンティティ（ヘッダー）
 */
public class Journal {
    private String journalNo;           // 仕訳伝票番号
    private LocalDate journalDate;      // 起票日
    private LocalDate inputDate;        // 入力日
    private Integer settlementFlag;     // 決算仕訳フラグ（0=通常、1=決算）
    private Integer singleEntryFlag;    // 単振フラグ（0=複合、1=単一）
    private Integer journalType;        // 仕訳伝票区分
    private Integer recurringFlag;      // 定期計上フラグ
    private String employeeCode;        // 社員コード
    private String departmentCode;      // 部門コード
    private Integer redSlipFlag;        // 赤伝フラグ（0=通常、1=赤伝）
    private String redBlackVoucherNo;   // 赤黒伝票番号
    private LocalDateTime createdAt;    // 作成日時
    private LocalDateTime updatedAt;    // 更新日時
    private List<JournalDetail> details; // 仕訳明細リスト

    public Journal() {
        this.details = new ArrayList<>();
    }

    public Journal(String journalNo, LocalDate journalDate, LocalDate inputDate) {
        this.journalNo = journalNo;
        this.journalDate = journalDate;
        this.inputDate = inputDate;
        this.settlementFlag = 0;
        this.singleEntryFlag = 0;
        this.journalType = 0;
        this.recurringFlag = 0;
        this.redSlipFlag = 0;
        this.details = new ArrayList<>();
    }

    // Getters
    public String getJournalNo() {
        return journalNo;
    }

    public LocalDate getJournalDate() {
        return journalDate;
    }

    public LocalDate getInputDate() {
        return inputDate;
    }

    public Integer getSettlementFlag() {
        return settlementFlag;
    }

    public Integer getSingleEntryFlag() {
        return singleEntryFlag;
    }

    public Integer getJournalType() {
        return journalType;
    }

    public Integer getRecurringFlag() {
        return recurringFlag;
    }

    public String getEmployeeCode() {
        return employeeCode;
    }

    public String getDepartmentCode() {
        return departmentCode;
    }

    public Integer getRedSlipFlag() {
        return redSlipFlag;
    }

    public String getRedBlackVoucherNo() {
        return redBlackVoucherNo;
    }

    public LocalDateTime getCreatedAt() {
        return createdAt;
    }

    public LocalDateTime getUpdatedAt() {
        return updatedAt;
    }

    public List<JournalDetail> getDetails() {
        return details;
    }

    // Setters
    public void setJournalNo(String journalNo) {
        this.journalNo = journalNo;
    }

    public void setJournalDate(LocalDate journalDate) {
        this.journalDate = journalDate;
    }

    public void setInputDate(LocalDate inputDate) {
        this.inputDate = inputDate;
    }

    public void setSettlementFlag(Integer settlementFlag) {
        this.settlementFlag = settlementFlag;
    }

    public void setSingleEntryFlag(Integer singleEntryFlag) {
        this.singleEntryFlag = singleEntryFlag;
    }

    public void setJournalType(Integer journalType) {
        this.journalType = journalType;
    }

    public void setRecurringFlag(Integer recurringFlag) {
        this.recurringFlag = recurringFlag;
    }

    public void setEmployeeCode(String employeeCode) {
        this.employeeCode = employeeCode;
    }

    public void setDepartmentCode(String departmentCode) {
        this.departmentCode = departmentCode;
    }

    public void setRedSlipFlag(Integer redSlipFlag) {
        this.redSlipFlag = redSlipFlag;
    }

    public void setRedBlackVoucherNo(String redBlackVoucherNo) {
        this.redBlackVoucherNo = redBlackVoucherNo;
    }

    public void setCreatedAt(LocalDateTime createdAt) {
        this.createdAt = createdAt;
    }

    public void setUpdatedAt(LocalDateTime updatedAt) {
        this.updatedAt = updatedAt;
    }

    public void setDetails(List<JournalDetail> details) {
        this.details = details;
    }

    // Business methods
    public void addDetail(JournalDetail detail) {
        this.details.add(detail);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Journal journal = (Journal) o;
        return Objects.equals(journalNo, journal.journalNo);
    }

    @Override
    public int hashCode() {
        return Objects.hash(journalNo);
    }

    @Override
    public String toString() {
        return "Journal{"
                + "journalNo='" + journalNo + '\''
                + ", journalDate=" + journalDate
                + ", inputDate=" + inputDate
                + ", settlementFlag=" + settlementFlag
                + ", singleEntryFlag=" + singleEntryFlag
                + ", journalType=" + journalType
                + ", recurringFlag=" + recurringFlag
                + ", employeeCode='" + employeeCode + '\''
                + ", departmentCode='" + departmentCode + '\''
                + ", redSlipFlag=" + redSlipFlag
                + ", redBlackVoucherNo='" + redBlackVoucherNo + '\''
                + ", createdAt=" + createdAt
                + ", updatedAt=" + updatedAt
                + ", details=" + details
                + '}';
    }
}
