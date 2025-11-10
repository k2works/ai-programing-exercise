package com.example.sales.domain.model;

import java.time.LocalDateTime;

/**
 * 与信残高データのEntityクラス
 */
public class CreditBalance {
    private String companyCode;
    private Integer orderBalance;
    private Integer receivableBalance;
    private Integer payableBalance;
    private LocalDateTime createdAt;
    private String createdBy;
    private LocalDateTime updatedAt;
    private String updatedBy;

    // デフォルトコンストラクタ
    public CreditBalance() {}

    // Getter/Setter（全フィールド）
    public String getCompanyCode() { return companyCode; }
    public void setCompanyCode(String companyCode) {
        this.companyCode = companyCode;
    }

    public Integer getOrderBalance() { return orderBalance; }
    public void setOrderBalance(Integer orderBalance) {
        this.orderBalance = orderBalance;
    }

    public Integer getReceivableBalance() { return receivableBalance; }
    public void setReceivableBalance(Integer receivableBalance) {
        this.receivableBalance = receivableBalance;
    }

    public Integer getPayableBalance() { return payableBalance; }
    public void setPayableBalance(Integer payableBalance) {
        this.payableBalance = payableBalance;
    }

    public LocalDateTime getCreatedAt() { return createdAt; }
    public void setCreatedAt(LocalDateTime createdAt) {
        this.createdAt = createdAt;
    }

    public String getCreatedBy() { return createdBy; }
    public void setCreatedBy(String createdBy) {
        this.createdBy = createdBy;
    }

    public LocalDateTime getUpdatedAt() { return updatedAt; }
    public void setUpdatedAt(LocalDateTime updatedAt) {
        this.updatedAt = updatedAt;
    }

    public String getUpdatedBy() { return updatedBy; }
    public void setUpdatedBy(String updatedBy) {
        this.updatedBy = updatedBy;
    }
}
