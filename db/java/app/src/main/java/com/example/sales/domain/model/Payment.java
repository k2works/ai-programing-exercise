package com.example.sales.domain.model;

import java.time.LocalDateTime;

/**
 * 支払データのEntityクラス
 */
public class Payment {
    private String paymentNo;
    private Integer paymentDate;
    private String departmentCode;
    private LocalDateTime startDate;
    private String supplierCode;
    private Integer supplierBranch;
    private Integer paymentMethodType;
    private Integer paymentAmount;
    private Integer consumptionTax;
    private Integer completeFlag;
    private LocalDateTime createdAt;
    private String createdBy;
    private LocalDateTime updatedAt;
    private String updatedBy;

    // デフォルトコンストラクタ
    public Payment() {}

    // Getter/Setter（全フィールド）
    public String getPaymentNo() { return paymentNo; }
    public void setPaymentNo(String paymentNo) { this.paymentNo = paymentNo; }

    public Integer getPaymentDate() { return paymentDate; }
    public void setPaymentDate(Integer paymentDate) { this.paymentDate = paymentDate; }

    public String getDepartmentCode() { return departmentCode; }
    public void setDepartmentCode(String departmentCode) {
        this.departmentCode = departmentCode;
    }

    public LocalDateTime getStartDate() { return startDate; }
    public void setStartDate(LocalDateTime startDate) { this.startDate = startDate; }

    public String getSupplierCode() { return supplierCode; }
    public void setSupplierCode(String supplierCode) {
        this.supplierCode = supplierCode;
    }

    public Integer getSupplierBranch() { return supplierBranch; }
    public void setSupplierBranch(Integer supplierBranch) {
        this.supplierBranch = supplierBranch;
    }

    public Integer getPaymentMethodType() { return paymentMethodType; }
    public void setPaymentMethodType(Integer paymentMethodType) {
        this.paymentMethodType = paymentMethodType;
    }

    public Integer getPaymentAmount() { return paymentAmount; }
    public void setPaymentAmount(Integer paymentAmount) {
        this.paymentAmount = paymentAmount;
    }

    public Integer getConsumptionTax() { return consumptionTax; }
    public void setConsumptionTax(Integer consumptionTax) {
        this.consumptionTax = consumptionTax;
    }

    public Integer getCompleteFlag() { return completeFlag; }
    public void setCompleteFlag(Integer completeFlag) {
        this.completeFlag = completeFlag;
    }

    public LocalDateTime getCreatedAt() { return createdAt; }
    public void setCreatedAt(LocalDateTime createdAt) { this.createdAt = createdAt; }

    public String getCreatedBy() { return createdBy; }
    public void setCreatedBy(String createdBy) { this.createdBy = createdBy; }

    public LocalDateTime getUpdatedAt() { return updatedAt; }
    public void setUpdatedAt(LocalDateTime updatedAt) { this.updatedAt = updatedAt; }

    public String getUpdatedBy() { return updatedBy; }
    public void setUpdatedBy(String updatedBy) { this.updatedBy = updatedBy; }
}
