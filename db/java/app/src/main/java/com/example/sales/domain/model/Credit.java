package com.example.sales.domain.model;

import java.time.LocalDateTime;

/**
 * 入金データのEntityクラス
 */
public class Credit {
    private String creditNo;
    private LocalDateTime creditDate;
    private String departmentCode;
    private LocalDateTime startDate;
    private String customerCode;
    private Integer customerBranch;
    private Integer paymentMethodType;
    private String bankAccountCode;
    private Integer creditAmount;
    private Integer appliedAmount;
    private LocalDateTime createdAt;
    private String createdBy;
    private LocalDateTime updatedAt;
    private String updatedBy;
    private LocalDateTime programUpdateDate;
    private String updateProgramName;

    // デフォルトコンストラクタ
    public Credit() {}

    // Getter/Setter（全フィールド）
    public String getCreditNo() { return creditNo; }
    public void setCreditNo(String creditNo) { this.creditNo = creditNo; }

    public LocalDateTime getCreditDate() { return creditDate; }
    public void setCreditDate(LocalDateTime creditDate) {
        this.creditDate = creditDate;
    }

    public String getDepartmentCode() { return departmentCode; }
    public void setDepartmentCode(String departmentCode) {
        this.departmentCode = departmentCode;
    }

    public LocalDateTime getStartDate() { return startDate; }
    public void setStartDate(LocalDateTime startDate) { this.startDate = startDate; }

    public String getCustomerCode() { return customerCode; }
    public void setCustomerCode(String customerCode) {
        this.customerCode = customerCode;
    }

    public Integer getCustomerBranch() { return customerBranch; }
    public void setCustomerBranch(Integer customerBranch) {
        this.customerBranch = customerBranch;
    }

    public Integer getPaymentMethodType() { return paymentMethodType; }
    public void setPaymentMethodType(Integer paymentMethodType) {
        this.paymentMethodType = paymentMethodType;
    }

    public String getBankAccountCode() { return bankAccountCode; }
    public void setBankAccountCode(String bankAccountCode) {
        this.bankAccountCode = bankAccountCode;
    }

    public Integer getCreditAmount() { return creditAmount; }
    public void setCreditAmount(Integer creditAmount) {
        this.creditAmount = creditAmount;
    }

    public Integer getAppliedAmount() { return appliedAmount; }
    public void setAppliedAmount(Integer appliedAmount) {
        this.appliedAmount = appliedAmount;
    }

    public LocalDateTime getCreatedAt() { return createdAt; }
    public void setCreatedAt(LocalDateTime createdAt) { this.createdAt = createdAt; }

    public String getCreatedBy() { return createdBy; }
    public void setCreatedBy(String createdBy) { this.createdBy = createdBy; }

    public LocalDateTime getUpdatedAt() { return updatedAt; }
    public void setUpdatedAt(LocalDateTime updatedAt) { this.updatedAt = updatedAt; }

    public String getUpdatedBy() { return updatedBy; }
    public void setUpdatedBy(String updatedBy) { this.updatedBy = updatedBy; }

    public LocalDateTime getProgramUpdateDate() { return programUpdateDate; }
    public void setProgramUpdateDate(LocalDateTime programUpdateDate) {
        this.programUpdateDate = programUpdateDate;
    }

    public String getUpdateProgramName() { return updateProgramName; }
    public void setUpdateProgramName(String updateProgramName) {
        this.updateProgramName = updateProgramName;
    }
}
