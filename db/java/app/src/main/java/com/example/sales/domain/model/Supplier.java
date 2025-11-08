package com.example.sales.domain.model;

import java.time.LocalDateTime;

/**
 * 仕入先マスタのEntityクラス
 */
public class Supplier {
    private String supplierCode;
    private Integer supplierBranch;
    private String supplierName;
    private String supplierNameKana;
    private String supplierUserName;
    private String supplierDepartmentName;
    private String supplierZipCode;
    private String supplierState;
    private String supplierAddress1;
    private String supplierAddress2;
    private String supplierTel;
    private String supplierFax;
    private String supplierEmail;
    private Integer supplierCloseDate;
    private Integer supplierPayMonths;
    private Integer supplierPayDates;
    private Integer supplierPayMethod;
    private LocalDateTime createdAt;
    private String createdBy;
    private LocalDateTime updatedAt;
    private String updatedBy;

    // デフォルトコンストラクタ
    public Supplier() {}

    // Getter/Setter
    public String getSupplierCode() { return supplierCode; }
    public void setSupplierCode(String supplierCode) { this.supplierCode = supplierCode; }

    public Integer getSupplierBranch() { return supplierBranch; }
    public void setSupplierBranch(Integer supplierBranch) { this.supplierBranch = supplierBranch; }

    public String getSupplierName() { return supplierName; }
    public void setSupplierName(String supplierName) { this.supplierName = supplierName; }

    public String getSupplierNameKana() { return supplierNameKana; }
    public void setSupplierNameKana(String supplierNameKana) {
        this.supplierNameKana = supplierNameKana;
    }

    public String getSupplierUserName() { return supplierUserName; }
    public void setSupplierUserName(String supplierUserName) {
        this.supplierUserName = supplierUserName;
    }

    public String getSupplierDepartmentName() { return supplierDepartmentName; }
    public void setSupplierDepartmentName(String supplierDepartmentName) {
        this.supplierDepartmentName = supplierDepartmentName;
    }

    public String getSupplierZipCode() { return supplierZipCode; }
    public void setSupplierZipCode(String supplierZipCode) {
        this.supplierZipCode = supplierZipCode;
    }

    public String getSupplierState() { return supplierState; }
    public void setSupplierState(String supplierState) { this.supplierState = supplierState; }

    public String getSupplierAddress1() { return supplierAddress1; }
    public void setSupplierAddress1(String supplierAddress1) {
        this.supplierAddress1 = supplierAddress1;
    }

    public String getSupplierAddress2() { return supplierAddress2; }
    public void setSupplierAddress2(String supplierAddress2) {
        this.supplierAddress2 = supplierAddress2;
    }

    public String getSupplierTel() { return supplierTel; }
    public void setSupplierTel(String supplierTel) { this.supplierTel = supplierTel; }

    public String getSupplierFax() { return supplierFax; }
    public void setSupplierFax(String supplierFax) { this.supplierFax = supplierFax; }

    public String getSupplierEmail() { return supplierEmail; }
    public void setSupplierEmail(String supplierEmail) { this.supplierEmail = supplierEmail; }

    public Integer getSupplierCloseDate() { return supplierCloseDate; }
    public void setSupplierCloseDate(Integer supplierCloseDate) {
        this.supplierCloseDate = supplierCloseDate;
    }

    public Integer getSupplierPayMonths() { return supplierPayMonths; }
    public void setSupplierPayMonths(Integer supplierPayMonths) {
        this.supplierPayMonths = supplierPayMonths;
    }

    public Integer getSupplierPayDates() { return supplierPayDates; }
    public void setSupplierPayDates(Integer supplierPayDates) {
        this.supplierPayDates = supplierPayDates;
    }

    public Integer getSupplierPayMethod() { return supplierPayMethod; }
    public void setSupplierPayMethod(Integer supplierPayMethod) {
        this.supplierPayMethod = supplierPayMethod;
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
