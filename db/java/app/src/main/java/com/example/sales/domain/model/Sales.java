package com.example.sales.domain.model;

import java.time.LocalDateTime;

/**
 * 売上データのEntityクラス
 */
public class Sales {
    private String salesNo;
    private String orderNo;
    private LocalDateTime salesDate;
    private Integer salesDivision;
    private String departmentCode;
    private LocalDateTime startDate;
    private String companyCode;
    private String employeeCode;
    private Integer salesAmount;
    private Integer consumptionTax;
    private String slipComment;
    private Integer redBlackSlipNo;
    private String originalSlipNo;
    private LocalDateTime createdAt;
    private String createdBy;
    private LocalDateTime updatedAt;
    private String updatedBy;

    // デフォルトコンストラクタ
    public Sales() {}

    // Getter/Setter（全フィールド）
    public String getSalesNo() { return salesNo; }
    public void setSalesNo(String salesNo) { this.salesNo = salesNo; }

    public String getOrderNo() { return orderNo; }
    public void setOrderNo(String orderNo) { this.orderNo = orderNo; }

    public LocalDateTime getSalesDate() { return salesDate; }
    public void setSalesDate(LocalDateTime salesDate) { this.salesDate = salesDate; }

    public Integer getSalesDivision() { return salesDivision; }
    public void setSalesDivision(Integer salesDivision) {
        this.salesDivision = salesDivision;
    }

    public String getDepartmentCode() { return departmentCode; }
    public void setDepartmentCode(String departmentCode) {
        this.departmentCode = departmentCode;
    }

    public LocalDateTime getStartDate() { return startDate; }
    public void setStartDate(LocalDateTime startDate) { this.startDate = startDate; }

    public String getCompanyCode() { return companyCode; }
    public void setCompanyCode(String companyCode) { this.companyCode = companyCode; }

    public String getEmployeeCode() { return employeeCode; }
    public void setEmployeeCode(String employeeCode) { this.employeeCode = employeeCode; }

    public Integer getSalesAmount() { return salesAmount; }
    public void setSalesAmount(Integer salesAmount) { this.salesAmount = salesAmount; }

    public Integer getConsumptionTax() { return consumptionTax; }
    public void setConsumptionTax(Integer consumptionTax) {
        this.consumptionTax = consumptionTax;
    }

    public String getSlipComment() { return slipComment; }
    public void setSlipComment(String slipComment) { this.slipComment = slipComment; }

    public Integer getRedBlackSlipNo() { return redBlackSlipNo; }
    public void setRedBlackSlipNo(Integer redBlackSlipNo) {
        this.redBlackSlipNo = redBlackSlipNo;
    }

    public String getOriginalSlipNo() { return originalSlipNo; }
    public void setOriginalSlipNo(String originalSlipNo) {
        this.originalSlipNo = originalSlipNo;
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
