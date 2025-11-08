package com.example.sales.domain.model;

import java.time.LocalDateTime;

/**
 * 発注データ明細のEntityクラス
 */
public class PurchaseOrderDetail {
    private String poNo;
    private Integer poLineNo;
    private String productCode;
    private String productName;
    private Integer poUnitPrice;
    private Integer poQuantity;
    private LocalDateTime expectedArrivalDate;
    private Integer receivedQuantity;
    private Integer completedFlag;
    private LocalDateTime createdAt;
    private String createdBy;
    private LocalDateTime updatedAt;
    private String updatedBy;

    // デフォルトコンストラクタ
    public PurchaseOrderDetail() {}

    // Getter/Setter（全フィールド）
    public String getPoNo() { return poNo; }
    public void setPoNo(String poNo) { this.poNo = poNo; }

    public Integer getPoLineNo() { return poLineNo; }
    public void setPoLineNo(Integer poLineNo) { this.poLineNo = poLineNo; }

    public String getProductCode() { return productCode; }
    public void setProductCode(String productCode) { this.productCode = productCode; }

    public String getProductName() { return productName; }
    public void setProductName(String productName) { this.productName = productName; }

    public Integer getPoUnitPrice() { return poUnitPrice; }
    public void setPoUnitPrice(Integer poUnitPrice) { this.poUnitPrice = poUnitPrice; }

    public Integer getPoQuantity() { return poQuantity; }
    public void setPoQuantity(Integer poQuantity) { this.poQuantity = poQuantity; }

    public LocalDateTime getExpectedArrivalDate() { return expectedArrivalDate; }
    public void setExpectedArrivalDate(LocalDateTime expectedArrivalDate) {
        this.expectedArrivalDate = expectedArrivalDate;
    }

    public Integer getReceivedQuantity() { return receivedQuantity; }
    public void setReceivedQuantity(Integer receivedQuantity) {
        this.receivedQuantity = receivedQuantity;
    }

    public Integer getCompletedFlag() { return completedFlag; }
    public void setCompletedFlag(Integer completedFlag) {
        this.completedFlag = completedFlag;
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
