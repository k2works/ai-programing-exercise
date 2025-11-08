package com.example.sales.domain.model;

import java.time.LocalDateTime;

/**
 * 在庫データのEntityクラス
 */
public class Stock {
    private String warehouseCode;
    private String productCode;
    private String lotNo;
    private String stockType;
    private String qualityType;
    private Integer actualQuantity;
    private Integer validQuantity;
    private LocalDateTime lastDeliveryDate;
    private LocalDateTime createdAt;
    private String createdBy;
    private LocalDateTime updatedAt;
    private String updatedBy;

    // デフォルトコンストラクタ
    public Stock() {}

    // Getter/Setter（全フィールド）
    public String getWarehouseCode() { return warehouseCode; }
    public void setWarehouseCode(String warehouseCode) {
        this.warehouseCode = warehouseCode;
    }

    public String getProductCode() { return productCode; }
    public void setProductCode(String productCode) { this.productCode = productCode; }

    public String getLotNo() { return lotNo; }
    public void setLotNo(String lotNo) { this.lotNo = lotNo; }

    public String getStockType() { return stockType; }
    public void setStockType(String stockType) { this.stockType = stockType; }

    public String getQualityType() { return qualityType; }
    public void setQualityType(String qualityType) { this.qualityType = qualityType; }

    public Integer getActualQuantity() { return actualQuantity; }
    public void setActualQuantity(Integer actualQuantity) {
        this.actualQuantity = actualQuantity;
    }

    public Integer getValidQuantity() { return validQuantity; }
    public void setValidQuantity(Integer validQuantity) {
        this.validQuantity = validQuantity;
    }

    public LocalDateTime getLastDeliveryDate() { return lastDeliveryDate; }
    public void setLastDeliveryDate(LocalDateTime lastDeliveryDate) {
        this.lastDeliveryDate = lastDeliveryDate;
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
