package com.example.accounting.domain.model;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;

/**
 * 仕訳ドメインモデル
 */
public class Journal {
    private String journalNo;
    private LocalDate journalDate;
    private LocalDate inputDate;
    private boolean settlementFlag;
    private boolean singleEntryFlag;
    private Integer journalType;
    private boolean recurringFlag;
    private String employeeCode;
    private String departmentCode;
    private boolean redSlipFlag;
    private String redBlackVoucherNo;
    private List<JournalEntry> entries;

    public Journal() {
        this.entries = new ArrayList<>();
    }

    @SuppressWarnings("checkstyle:ParameterNumber")
    public Journal(String journalNo, LocalDate journalDate, LocalDate inputDate,
                   boolean settlementFlag, boolean singleEntryFlag, Integer journalType,
                   boolean recurringFlag, String employeeCode, String departmentCode,
                   boolean redSlipFlag, String redBlackVoucherNo) {
        this.journalNo = journalNo;
        this.journalDate = journalDate;
        this.inputDate = inputDate;
        this.settlementFlag = settlementFlag;
        this.singleEntryFlag = singleEntryFlag;
        this.journalType = journalType;
        this.recurringFlag = recurringFlag;
        this.employeeCode = employeeCode;
        this.departmentCode = departmentCode;
        this.redSlipFlag = redSlipFlag;
        this.redBlackVoucherNo = redBlackVoucherNo;
        this.entries = new ArrayList<>();
    }

    /**
     * 仕訳明細を追加
     */
    public void addEntry(JournalEntry entry) {
        this.entries.add(entry);
    }

    /**
     * 決算仕訳かどうか
     */
    public boolean isSettlementJournal() {
        return settlementFlag;
    }

    /**
     * 単一仕訳かどうか
     */
    public boolean isSingleEntry() {
        return singleEntryFlag;
    }

    /**
     * 赤伝かどうか
     */
    public boolean isRedSlip() {
        return redSlipFlag;
    }

    public String getJournalNo() {
        return journalNo;
    }

    public void setJournalNo(String journalNo) {
        this.journalNo = journalNo;
    }

    public LocalDate getJournalDate() {
        return journalDate;
    }

    public void setJournalDate(LocalDate journalDate) {
        this.journalDate = journalDate;
    }

    public LocalDate getInputDate() {
        return inputDate;
    }

    public void setInputDate(LocalDate inputDate) {
        this.inputDate = inputDate;
    }

    public boolean isSettlementFlag() {
        return settlementFlag;
    }

    public void setSettlementFlag(boolean settlementFlag) {
        this.settlementFlag = settlementFlag;
    }

    public boolean isSingleEntryFlag() {
        return singleEntryFlag;
    }

    public void setSingleEntryFlag(boolean singleEntryFlag) {
        this.singleEntryFlag = singleEntryFlag;
    }

    public Integer getJournalType() {
        return journalType;
    }

    public void setJournalType(Integer journalType) {
        this.journalType = journalType;
    }

    public boolean isRecurringFlag() {
        return recurringFlag;
    }

    public void setRecurringFlag(boolean recurringFlag) {
        this.recurringFlag = recurringFlag;
    }

    public String getEmployeeCode() {
        return employeeCode;
    }

    public void setEmployeeCode(String employeeCode) {
        this.employeeCode = employeeCode;
    }

    public String getDepartmentCode() {
        return departmentCode;
    }

    public void setDepartmentCode(String departmentCode) {
        this.departmentCode = departmentCode;
    }

    public boolean isRedSlipFlag() {
        return redSlipFlag;
    }

    public void setRedSlipFlag(boolean redSlipFlag) {
        this.redSlipFlag = redSlipFlag;
    }

    public String getRedBlackVoucherNo() {
        return redBlackVoucherNo;
    }

    public void setRedBlackVoucherNo(String redBlackVoucherNo) {
        this.redBlackVoucherNo = redBlackVoucherNo;
    }

    public List<JournalEntry> getEntries() {
        return entries;
    }

    public void setEntries(List<JournalEntry> entries) {
        this.entries = entries;
    }
}
