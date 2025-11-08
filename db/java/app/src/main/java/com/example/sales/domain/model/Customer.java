package com.example.sales.domain.model;

import java.time.LocalDateTime;

/**
 * 顧客マスタのEntityクラス
 */
public class Customer {
    private String customerCode;
    private Integer customerBranch;
    private Integer customerType;
    private String arCode;
    private Integer arBranch;
    private String payerCode;
    private Integer payerBranch;
    private String customerName;
    private String customerNameKana;
    private String employeeCode;
    private String customerUserName;
    private String customerDepartmentName;
    private String customerZipCode;
    private String customerState;
    private String customerAddress1;
    private String customerAddress2;
    private String customerTel;
    private String customerFax;
    private String customerEmail;
    private Integer customerArType;
    private Integer customerCloseDate1;
    private Integer customerPayMonths1;
    private Integer customerPayDates1;
    private Integer customerPayMethod1;
    private Integer customerCloseDate2;
    private Integer customerPayMonths2;
    private Integer customerPayDates2;
    private Integer customerPayMethod2;
    private LocalDateTime createdAt;
    private String createdBy;
    private LocalDateTime updatedAt;
    private String updatedBy;

    // デフォルトコンストラクタ
    public Customer() {}

    // Getter/Setter（全フィールド）
    public String getCustomerCode() { return customerCode; }
    public void setCustomerCode(String customerCode) { this.customerCode = customerCode; }

    public Integer getCustomerBranch() { return customerBranch; }
    public void setCustomerBranch(Integer customerBranch) {
        this.customerBranch = customerBranch;
    }

    public Integer getCustomerType() { return customerType; }
    public void setCustomerType(Integer customerType) { this.customerType = customerType; }

    public String getArCode() { return arCode; }
    public void setArCode(String arCode) { this.arCode = arCode; }

    public Integer getArBranch() { return arBranch; }
    public void setArBranch(Integer arBranch) { this.arBranch = arBranch; }

    public String getPayerCode() { return payerCode; }
    public void setPayerCode(String payerCode) { this.payerCode = payerCode; }

    public Integer getPayerBranch() { return payerBranch; }
    public void setPayerBranch(Integer payerBranch) { this.payerBranch = payerBranch; }

    public String getCustomerName() { return customerName; }
    public void setCustomerName(String customerName) { this.customerName = customerName; }

    public String getCustomerNameKana() { return customerNameKana; }
    public void setCustomerNameKana(String customerNameKana) {
        this.customerNameKana = customerNameKana;
    }

    public String getEmployeeCode() { return employeeCode; }
    public void setEmployeeCode(String employeeCode) { this.employeeCode = employeeCode; }

    public String getCustomerUserName() { return customerUserName; }
    public void setCustomerUserName(String customerUserName) {
        this.customerUserName = customerUserName;
    }

    public String getCustomerDepartmentName() { return customerDepartmentName; }
    public void setCustomerDepartmentName(String customerDepartmentName) {
        this.customerDepartmentName = customerDepartmentName;
    }

    public String getCustomerZipCode() { return customerZipCode; }
    public void setCustomerZipCode(String customerZipCode) {
        this.customerZipCode = customerZipCode;
    }

    public String getCustomerState() { return customerState; }
    public void setCustomerState(String customerState) { this.customerState = customerState; }

    public String getCustomerAddress1() { return customerAddress1; }
    public void setCustomerAddress1(String customerAddress1) {
        this.customerAddress1 = customerAddress1;
    }

    public String getCustomerAddress2() { return customerAddress2; }
    public void setCustomerAddress2(String customerAddress2) {
        this.customerAddress2 = customerAddress2;
    }

    public String getCustomerTel() { return customerTel; }
    public void setCustomerTel(String customerTel) { this.customerTel = customerTel; }

    public String getCustomerFax() { return customerFax; }
    public void setCustomerFax(String customerFax) { this.customerFax = customerFax; }

    public String getCustomerEmail() { return customerEmail; }
    public void setCustomerEmail(String customerEmail) { this.customerEmail = customerEmail; }

    public Integer getCustomerArType() { return customerArType; }
    public void setCustomerArType(Integer customerArType) {
        this.customerArType = customerArType;
    }

    public Integer getCustomerCloseDate1() { return customerCloseDate1; }
    public void setCustomerCloseDate1(Integer customerCloseDate1) {
        this.customerCloseDate1 = customerCloseDate1;
    }

    public Integer getCustomerPayMonths1() { return customerPayMonths1; }
    public void setCustomerPayMonths1(Integer customerPayMonths1) {
        this.customerPayMonths1 = customerPayMonths1;
    }

    public Integer getCustomerPayDates1() { return customerPayDates1; }
    public void setCustomerPayDates1(Integer customerPayDates1) {
        this.customerPayDates1 = customerPayDates1;
    }

    public Integer getCustomerPayMethod1() { return customerPayMethod1; }
    public void setCustomerPayMethod1(Integer customerPayMethod1) {
        this.customerPayMethod1 = customerPayMethod1;
    }

    public Integer getCustomerCloseDate2() { return customerCloseDate2; }
    public void setCustomerCloseDate2(Integer customerCloseDate2) {
        this.customerCloseDate2 = customerCloseDate2;
    }

    public Integer getCustomerPayMonths2() { return customerPayMonths2; }
    public void setCustomerPayMonths2(Integer customerPayMonths2) {
        this.customerPayMonths2 = customerPayMonths2;
    }

    public Integer getCustomerPayDates2() { return customerPayDates2; }
    public void setCustomerPayDates2(Integer customerPayDates2) {
        this.customerPayDates2 = customerPayDates2;
    }

    public Integer getCustomerPayMethod2() { return customerPayMethod2; }
    public void setCustomerPayMethod2(Integer customerPayMethod2) {
        this.customerPayMethod2 = customerPayMethod2;
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
