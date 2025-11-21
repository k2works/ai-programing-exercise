package com.example.accounting.application.model;

import java.math.BigDecimal;
import java.time.LocalDate;

/**
 * 日次残高ドメインモデル
 */
public class DailyBalance {

    private LocalDate entryDate;
    private String accountCode;
    private String subAccountCode;
    private String departmentCode;
    private String projectCode;
    private boolean isSettlement;
    private BigDecimal debitAmount;
    private BigDecimal creditAmount;

    public DailyBalance() {
        this.subAccountCode = "";
        this.departmentCode = "";
        this.projectCode = "";
        this.isSettlement = false;
        this.debitAmount = BigDecimal.ZERO;
        this.creditAmount = BigDecimal.ZERO;
    }

    /**
     * 残高（借方 - 貸方）を計算
     *
     * @return 残高
     */
    public BigDecimal getBalance() {
        return debitAmount.subtract(creditAmount);
    }

    /**
     * 純額（絶対値）を計算
     *
     * @return 純額
     */
    public BigDecimal getNetAmount() {
        return getBalance().abs();
    }

    // Getters and Setters

    public LocalDate getEntryDate() {
        return entryDate;
    }

    public void setEntryDate(LocalDate entryDate) {
        this.entryDate = entryDate;
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
        this.subAccountCode = subAccountCode != null ? subAccountCode : "";
    }

    public String getDepartmentCode() {
        return departmentCode;
    }

    public void setDepartmentCode(String departmentCode) {
        this.departmentCode = departmentCode != null ? departmentCode : "";
    }

    public String getProjectCode() {
        return projectCode;
    }

    public void setProjectCode(String projectCode) {
        this.projectCode = projectCode != null ? projectCode : "";
    }

    public boolean isSettlement() {
        return isSettlement;
    }

    public void setSettlement(boolean settlement) {
        isSettlement = settlement;
    }

    public BigDecimal getDebitAmount() {
        return debitAmount;
    }

    public void setDebitAmount(BigDecimal debitAmount) {
        this.debitAmount = debitAmount != null ? debitAmount : BigDecimal.ZERO;
    }

    public BigDecimal getCreditAmount() {
        return creditAmount;
    }

    public void setCreditAmount(BigDecimal creditAmount) {
        this.creditAmount = creditAmount != null ? creditAmount : BigDecimal.ZERO;
    }
}
