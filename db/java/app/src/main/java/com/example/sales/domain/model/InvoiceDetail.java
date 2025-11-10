package com.example.sales.domain.model;

import java.time.LocalDateTime;

/**
 * 請求データ明細のEntityクラス
 */
public class InvoiceDetail {
    private String invoiceNo;
    private Integer invoiceLineNo;
    private String salesNo;
    private Integer salesLineNo;
    private Integer invoiceAmount;
    private LocalDateTime createdAt;
    private String createdBy;
    private LocalDateTime updatedAt;
    private String updatedBy;

    // デフォルトコンストラクタ
    public InvoiceDetail() {}

    // Getter/Setter（全フィールド）
    public String getInvoiceNo() { return invoiceNo; }
    public void setInvoiceNo(String invoiceNo) { this.invoiceNo = invoiceNo; }

    public Integer getInvoiceLineNo() { return invoiceLineNo; }
    public void setInvoiceLineNo(Integer invoiceLineNo) {
        this.invoiceLineNo = invoiceLineNo;
    }

    public String getSalesNo() { return salesNo; }
    public void setSalesNo(String salesNo) { this.salesNo = salesNo; }

    public Integer getSalesLineNo() { return salesLineNo; }
    public void setSalesLineNo(Integer salesLineNo) {
        this.salesLineNo = salesLineNo;
    }

    public Integer getInvoiceAmount() { return invoiceAmount; }
    public void setInvoiceAmount(Integer invoiceAmount) {
        this.invoiceAmount = invoiceAmount;
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
