package com.example.sales.domain.model;

import java.time.LocalDateTime;

/**
 * 発注データのEntityクラス
 */
public class PurchaseOrder {
    private String poNo;
    private LocalDateTime poDate;
    private String orderNo;
    private String supplierCode;
    private Integer supplierBranch;
    private String employeeCode;
    private LocalDateTime dueDate;
    private String warehouseCode;
    private Integer poAmount;
    private Integer consumptionTax;
    private String slipComment;
    private LocalDateTime createdAt;
    private String createdBy;
    private LocalDateTime updatedAt;
    private String updatedBy;

    // デフォルトコンストラクタ
    public PurchaseOrder() {}

    // Getter/Setter（全フィールド）
    public String getPoNo() { return poNo; }
    public void setPoNo(String poNo) { this.poNo = poNo; }

    public LocalDateTime getPoDate() { return poDate; }
    public void setPoDate(LocalDateTime poDate) { this.poDate = poDate; }

    public String getOrderNo() { return orderNo; }
    public void setOrderNo(String orderNo) { this.orderNo = orderNo; }

    public String getSupplierCode() { return supplierCode; }
    public void setSupplierCode(String supplierCode) {
        this.supplierCode = supplierCode;
    }

    public Integer getSupplierBranch() { return supplierBranch; }
    public void setSupplierBranch(Integer supplierBranch) {
        this.supplierBranch = supplierBranch;
    }

    public String getEmployeeCode() { return employeeCode; }
    public void setEmployeeCode(String employeeCode) {
        this.employeeCode = employeeCode;
    }

    public LocalDateTime getDueDate() { return dueDate; }
    public void setDueDate(LocalDateTime dueDate) { this.dueDate = dueDate; }

    public String getWarehouseCode() { return warehouseCode; }
    public void setWarehouseCode(String warehouseCode) {
        this.warehouseCode = warehouseCode;
    }

    public Integer getPoAmount() { return poAmount; }
    public void setPoAmount(Integer poAmount) { this.poAmount = poAmount; }

    public Integer getConsumptionTax() { return consumptionTax; }
    public void setConsumptionTax(Integer consumptionTax) {
        this.consumptionTax = consumptionTax;
    }

    public String getSlipComment() { return slipComment; }
    public void setSlipComment(String slipComment) { this.slipComment = slipComment; }

    public LocalDateTime getCreatedAt() { return createdAt; }
    public void setCreatedAt(LocalDateTime createdAt) { this.createdAt = createdAt; }

    public String getCreatedBy() { return createdBy; }
    public void setCreatedBy(String createdBy) { this.createdBy = createdBy; }

    public LocalDateTime getUpdatedAt() { return updatedAt; }
    public void setUpdatedAt(LocalDateTime updatedAt) { this.updatedAt = updatedAt; }

    public String getUpdatedBy() { return updatedBy; }
    public void setUpdatedBy(String updatedBy) { this.updatedBy = updatedBy; }
}
