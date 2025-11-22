package com.example.accounting.domain.model.financial;

import java.math.BigDecimal;
import java.time.LocalDate;

/**
 * 仕訳貸借明細ドメインモデル
 */
public class JournalLine {
    private String debitCreditFlag;
    private String currencyCode;
    private BigDecimal exchangeRate;
    private String departmentCode;
    private String projectCode;
    private String accountCode;
    private String subAccountCode;
    private BigDecimal amount;
    private BigDecimal baseAmount;
    private String taxType;
    private Integer taxRate;
    private String taxCalcType;
    private LocalDate dueDate;
    private boolean cashFlowFlag;
    private String segmentCode;
    private String offsetAccountCode;
    private String offsetSubAccountCode;
    private String noteCode;
    private String noteContent;

    public JournalLine() {
    }

    @SuppressWarnings("checkstyle:ParameterNumber")
    public JournalLine(String debitCreditFlag, String accountCode, BigDecimal amount,
                       String currencyCode, BigDecimal exchangeRate) {
        this.debitCreditFlag = debitCreditFlag;
        this.accountCode = accountCode;
        this.amount = amount;
        this.currencyCode = currencyCode;
        this.exchangeRate = exchangeRate;
        this.baseAmount = amount;
        this.cashFlowFlag = false;
    }

    /**
     * 借方かどうか
     */
    public boolean isDebit() {
        return "D".equals(debitCreditFlag);
    }

    /**
     * 貸方かどうか
     */
    public boolean isCredit() {
        return "C".equals(debitCreditFlag);
    }

    public String getDebitCreditFlag() {
        return debitCreditFlag;
    }

    public void setDebitCreditFlag(String debitCreditFlag) {
        this.debitCreditFlag = debitCreditFlag;
    }

    public String getCurrencyCode() {
        return currencyCode;
    }

    public void setCurrencyCode(String currencyCode) {
        this.currencyCode = currencyCode;
    }

    public BigDecimal getExchangeRate() {
        return exchangeRate;
    }

    public void setExchangeRate(BigDecimal exchangeRate) {
        this.exchangeRate = exchangeRate;
    }

    public String getDepartmentCode() {
        return departmentCode;
    }

    public void setDepartmentCode(String departmentCode) {
        this.departmentCode = departmentCode;
    }

    public String getProjectCode() {
        return projectCode;
    }

    public void setProjectCode(String projectCode) {
        this.projectCode = projectCode;
    }

    public String getAccountCode() {
        return accountCode;
    }

    public void setAccountCode(String accountCode) {
        this.accountCode = accountCode;
    }

    public String getSubAccountCode() {
        return subAccountCode;
    }

    public void setSubAccountCode(String subAccountCode) {
        this.subAccountCode = subAccountCode;
    }

    public BigDecimal getAmount() {
        return amount;
    }

    public void setAmount(BigDecimal amount) {
        this.amount = amount;
    }

    public BigDecimal getBaseAmount() {
        return baseAmount;
    }

    public void setBaseAmount(BigDecimal baseAmount) {
        this.baseAmount = baseAmount;
    }

    public String getTaxType() {
        return taxType;
    }

    public void setTaxType(String taxType) {
        this.taxType = taxType;
    }

    public Integer getTaxRate() {
        return taxRate;
    }

    public void setTaxRate(Integer taxRate) {
        this.taxRate = taxRate;
    }

    public String getTaxCalcType() {
        return taxCalcType;
    }

    public void setTaxCalcType(String taxCalcType) {
        this.taxCalcType = taxCalcType;
    }

    public LocalDate getDueDate() {
        return dueDate;
    }

    public void setDueDate(LocalDate dueDate) {
        this.dueDate = dueDate;
    }

    public boolean isCashFlowFlag() {
        return cashFlowFlag;
    }

    public void setCashFlowFlag(boolean cashFlowFlag) {
        this.cashFlowFlag = cashFlowFlag;
    }

    public String getSegmentCode() {
        return segmentCode;
    }

    public void setSegmentCode(String segmentCode) {
        this.segmentCode = segmentCode;
    }

    public String getOffsetAccountCode() {
        return offsetAccountCode;
    }

    public void setOffsetAccountCode(String offsetAccountCode) {
        this.offsetAccountCode = offsetAccountCode;
    }

    public String getOffsetSubAccountCode() {
        return offsetSubAccountCode;
    }

    public void setOffsetSubAccountCode(String offsetSubAccountCode) {
        this.offsetSubAccountCode = offsetSubAccountCode;
    }

    public String getNoteCode() {
        return noteCode;
    }

    public void setNoteCode(String noteCode) {
        this.noteCode = noteCode;
    }

    public String getNoteContent() {
        return noteContent;
    }

    public void setNoteContent(String noteContent) {
        this.noteContent = noteContent;
    }
}
