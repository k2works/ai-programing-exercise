package com.example.sales.domain.model;

import java.time.LocalDateTime;

/**
 * 受注データ明細のEntityクラス
 */
public class OrderDetail {
    private String orderNo;
    private Integer orderRowNo;
    private String productCode;
    private String productName;
    private Integer unitPrice;
    private Integer quantity;
    private Integer consumptionTaxRate;
    private Integer reserveQuantity;
    private Integer deliveryOrderQuantity;
    private Integer deliveredQuantity;
    private Integer completeFlag;
    private Integer discount;
    private LocalDateTime deliveryDate;
    private LocalDateTime createdAt;
    private String createdBy;
    private LocalDateTime updatedAt;
    private String updatedBy;

    // デフォルトコンストラクタ
    public OrderDetail() {}

    // Getter/Setter（全フィールド - 省略形）
    public String getOrderNo() { return orderNo; }
    public void setOrderNo(String orderNo) { this.orderNo = orderNo; }

    public Integer getOrderRowNo() { return orderRowNo; }
    public void setOrderRowNo(Integer orderRowNo) { this.orderRowNo = orderRowNo; }

    public String getProductCode() { return productCode; }
    public void setProductCode(String productCode) { this.productCode = productCode; }

    public String getProductName() { return productName; }
    public void setProductName(String productName) { this.productName = productName; }

    public Integer getUnitPrice() { return unitPrice; }
    public void setUnitPrice(Integer unitPrice) { this.unitPrice = unitPrice; }

    public Integer getQuantity() { return quantity; }
    public void setQuantity(Integer quantity) { this.quantity = quantity; }

    public Integer getConsumptionTaxRate() { return consumptionTaxRate; }
    public void setConsumptionTaxRate(Integer consumptionTaxRate) {
        this.consumptionTaxRate = consumptionTaxRate;
    }

    public Integer getReserveQuantity() { return reserveQuantity; }
    public void setReserveQuantity(Integer reserveQuantity) {
        this.reserveQuantity = reserveQuantity;
    }

    public Integer getDeliveryOrderQuantity() { return deliveryOrderQuantity; }
    public void setDeliveryOrderQuantity(Integer deliveryOrderQuantity) {
        this.deliveryOrderQuantity = deliveryOrderQuantity;
    }

    public Integer getDeliveredQuantity() { return deliveredQuantity; }
    public void setDeliveredQuantity(Integer deliveredQuantity) {
        this.deliveredQuantity = deliveredQuantity;
    }

    public Integer getCompleteFlag() { return completeFlag; }
    public void setCompleteFlag(Integer completeFlag) { this.completeFlag = completeFlag; }

    public Integer getDiscount() { return discount; }
    public void setDiscount(Integer discount) { this.discount = discount; }

    public LocalDateTime getDeliveryDate() { return deliveryDate; }
    public void setDeliveryDate(LocalDateTime deliveryDate) {
        this.deliveryDate = deliveryDate;
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
