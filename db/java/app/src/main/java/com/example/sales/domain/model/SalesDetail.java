package com.example.sales.domain.model;

import java.time.LocalDateTime;

/**
 * 売上データ明細のEntityクラス
 */
public class SalesDetail {
    private String salesNo;
    private Integer salesRowNo;
    private String productCode;
    private String productName;
    private Integer unitPrice;
    private Integer shippedQuantity;
    private Integer salesQuantity;
    private Integer discount;
    private LocalDateTime invoiceDate;
    private String invoiceNo;
    private Integer invoiceDelayDivision;
    private LocalDateTime autoJournalDate;
    private LocalDateTime createdAt;
    private String createdBy;
    private LocalDateTime updatedAt;
    private String updatedBy;

    // デフォルトコンストラクタ
    public SalesDetail() {}

    // Getter/Setter（全フィールド）
    public String getSalesNo() { return salesNo; }
    public void setSalesNo(String salesNo) { this.salesNo = salesNo; }

    public Integer getSalesRowNo() { return salesRowNo; }
    public void setSalesRowNo(Integer salesRowNo) { this.salesRowNo = salesRowNo; }

    public String getProductCode() { return productCode; }
    public void setProductCode(String productCode) { this.productCode = productCode; }

    public String getProductName() { return productName; }
    public void setProductName(String productName) { this.productName = productName; }

    public Integer getUnitPrice() { return unitPrice; }
    public void setUnitPrice(Integer unitPrice) { this.unitPrice = unitPrice; }

    public Integer getShippedQuantity() { return shippedQuantity; }
    public void setShippedQuantity(Integer shippedQuantity) {
        this.shippedQuantity = shippedQuantity;
    }

    public Integer getSalesQuantity() { return salesQuantity; }
    public void setSalesQuantity(Integer salesQuantity) {
        this.salesQuantity = salesQuantity;
    }

    public Integer getDiscount() { return discount; }
    public void setDiscount(Integer discount) { this.discount = discount; }

    public LocalDateTime getInvoiceDate() { return invoiceDate; }
    public void setInvoiceDate(LocalDateTime invoiceDate) {
        this.invoiceDate = invoiceDate;
    }

    public String getInvoiceNo() { return invoiceNo; }
    public void setInvoiceNo(String invoiceNo) { this.invoiceNo = invoiceNo; }

    public Integer getInvoiceDelayDivision() { return invoiceDelayDivision; }
    public void setInvoiceDelayDivision(Integer invoiceDelayDivision) {
        this.invoiceDelayDivision = invoiceDelayDivision;
    }

    public LocalDateTime getAutoJournalDate() { return autoJournalDate; }
    public void setAutoJournalDate(LocalDateTime autoJournalDate) {
        this.autoJournalDate = autoJournalDate;
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
