package com.example.sales.domain.model;

import java.time.LocalDateTime;

/**
 * 受注データのEntityクラス
 */
public class Order {
    private String orderNo;
    private LocalDateTime orderDate;
    private String departmentCode;
    private LocalDateTime startDate;
    private String customerCode;
    private Integer customerBranch;
    private String employeeCode;
    private LocalDateTime requiredDate;
    private String customerOrderNo;
    private String warehouseCode;
    private Integer orderAmount;
    private Integer consumptionTax;
    private String slipComment;
    private LocalDateTime createdAt;
    private String createdBy;
    private LocalDateTime updatedAt;
    private String updatedBy;

    // デフォルトコンストラクタ
    public Order() {}

    // Getter/Setter（全フィールド）
    public String getOrderNo() { return orderNo; }
    public void setOrderNo(String orderNo) { this.orderNo = orderNo; }

    public LocalDateTime getOrderDate() { return orderDate; }
    public void setOrderDate(LocalDateTime orderDate) { this.orderDate = orderDate; }

    public String getDepartmentCode() { return departmentCode; }
    public void setDepartmentCode(String departmentCode) {
        this.departmentCode = departmentCode;
    }

    public LocalDateTime getStartDate() { return startDate; }
    public void setStartDate(LocalDateTime startDate) { this.startDate = startDate; }

    public String getCustomerCode() { return customerCode; }
    public void setCustomerCode(String customerCode) { this.customerCode = customerCode; }

    public Integer getCustomerBranch() { return customerBranch; }
    public void setCustomerBranch(Integer customerBranch) {
        this.customerBranch = customerBranch;
    }

    public String getEmployeeCode() { return employeeCode; }
    public void setEmployeeCode(String employeeCode) { this.employeeCode = employeeCode; }

    public LocalDateTime getRequiredDate() { return requiredDate; }
    public void setRequiredDate(LocalDateTime requiredDate) {
        this.requiredDate = requiredDate;
    }

    public String getCustomerOrderNo() { return customerOrderNo; }
    public void setCustomerOrderNo(String customerOrderNo) {
        this.customerOrderNo = customerOrderNo;
    }

    public String getWarehouseCode() { return warehouseCode; }
    public void setWarehouseCode(String warehouseCode) {
        this.warehouseCode = warehouseCode;
    }

    public Integer getOrderAmount() { return orderAmount; }
    public void setOrderAmount(Integer orderAmount) { this.orderAmount = orderAmount; }

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
