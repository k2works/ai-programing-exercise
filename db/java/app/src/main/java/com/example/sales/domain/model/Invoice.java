package com.example.sales.domain.model;

import java.time.LocalDateTime;

/**
 * 請求データのEntityクラス
 */
public class Invoice {
    private String invoiceNo;
    private LocalDateTime invoiceDate;
    private String companyCode;
    private Integer customerBranch;
    private Integer lastReceivedAmount;
    private Integer monthSalesAmount;
    private Integer monthReceivedAmount;
    private Integer monthInvoiceAmount;
    private Integer consumptionTax;
    private Integer invoiceReceivedAmount;
    private LocalDateTime createdAt;
    private String createdBy;
    private LocalDateTime updatedAt;
    private String updatedBy;

    // デフォルトコンストラクタ
    public Invoice() {}

    // Getter/Setter（全フィールド）
    public String getInvoiceNo() { return invoiceNo; }
    public void setInvoiceNo(String invoiceNo) { this.invoiceNo = invoiceNo; }

    public LocalDateTime getInvoiceDate() { return invoiceDate; }
    public void setInvoiceDate(LocalDateTime invoiceDate) {
        this.invoiceDate = invoiceDate;
    }

    public String getCompanyCode() { return companyCode; }
    public void setCompanyCode(String companyCode) { this.companyCode = companyCode; }

    public Integer getCustomerBranch() { return customerBranch; }
    public void setCustomerBranch(Integer customerBranch) {
        this.customerBranch = customerBranch;
    }

    public Integer getLastReceivedAmount() { return lastReceivedAmount; }
    public void setLastReceivedAmount(Integer lastReceivedAmount) {
        this.lastReceivedAmount = lastReceivedAmount;
    }

    public Integer getMonthSalesAmount() { return monthSalesAmount; }
    public void setMonthSalesAmount(Integer monthSalesAmount) {
        this.monthSalesAmount = monthSalesAmount;
    }

    public Integer getMonthReceivedAmount() { return monthReceivedAmount; }
    public void setMonthReceivedAmount(Integer monthReceivedAmount) {
        this.monthReceivedAmount = monthReceivedAmount;
    }

    public Integer getMonthInvoiceAmount() { return monthInvoiceAmount; }
    public void setMonthInvoiceAmount(Integer monthInvoiceAmount) {
        this.monthInvoiceAmount = monthInvoiceAmount;
    }

    public Integer getConsumptionTax() { return consumptionTax; }
    public void setConsumptionTax(Integer consumptionTax) {
        this.consumptionTax = consumptionTax;
    }

    public Integer getInvoiceReceivedAmount() { return invoiceReceivedAmount; }
    public void setInvoiceReceivedAmount(Integer invoiceReceivedAmount) {
        this.invoiceReceivedAmount = invoiceReceivedAmount;
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
