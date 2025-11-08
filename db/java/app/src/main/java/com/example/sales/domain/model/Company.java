package com.example.sales.domain.model;

import java.time.LocalDateTime;

/**
 * 取引先マスタのEntityクラス
 */
public class Company {
    private String companyCode;
    private String companyName;
    private String companyNameKana;
    private Integer supplierType;
    private String zipCode;
    private String state;
    private String address1;
    private String address2;
    private Integer noSalesFlag;
    private Integer wideUseType;
    private String companyGroupCode;
    private Integer maxCredit;
    private Integer tempCreditUp;
    private LocalDateTime createdAt;
    private String createdBy;
    private LocalDateTime updatedAt;
    private String updatedBy;

    // デフォルトコンストラクタ
    public Company() {}

    // Getter/Setter
    public String getCompanyCode() { return companyCode; }
    public void setCompanyCode(String companyCode) { this.companyCode = companyCode; }

    public String getCompanyName() { return companyName; }
    public void setCompanyName(String companyName) { this.companyName = companyName; }

    public String getCompanyNameKana() { return companyNameKana; }
    public void setCompanyNameKana(String companyNameKana) {
        this.companyNameKana = companyNameKana;
    }

    public Integer getSupplierType() { return supplierType; }
    public void setSupplierType(Integer supplierType) { this.supplierType = supplierType; }

    public String getZipCode() { return zipCode; }
    public void setZipCode(String zipCode) { this.zipCode = zipCode; }

    public String getState() { return state; }
    public void setState(String state) { this.state = state; }

    public String getAddress1() { return address1; }
    public void setAddress1(String address1) { this.address1 = address1; }

    public String getAddress2() { return address2; }
    public void setAddress2(String address2) { this.address2 = address2; }

    public Integer getNoSalesFlag() { return noSalesFlag; }
    public void setNoSalesFlag(Integer noSalesFlag) { this.noSalesFlag = noSalesFlag; }

    public Integer getWideUseType() { return wideUseType; }
    public void setWideUseType(Integer wideUseType) { this.wideUseType = wideUseType; }

    public String getCompanyGroupCode() { return companyGroupCode; }
    public void setCompanyGroupCode(String companyGroupCode) {
        this.companyGroupCode = companyGroupCode;
    }

    public Integer getMaxCredit() { return maxCredit; }
    public void setMaxCredit(Integer maxCredit) { this.maxCredit = maxCredit; }

    public Integer getTempCreditUp() { return tempCreditUp; }
    public void setTempCreditUp(Integer tempCreditUp) { this.tempCreditUp = tempCreditUp; }

    public LocalDateTime getCreatedAt() { return createdAt; }
    public void setCreatedAt(LocalDateTime createdAt) { this.createdAt = createdAt; }

    public String getCreatedBy() { return createdBy; }
    public void setCreatedBy(String createdBy) { this.createdBy = createdBy; }

    public LocalDateTime getUpdatedAt() { return updatedAt; }
    public void setUpdatedAt(LocalDateTime updatedAt) { this.updatedAt = updatedAt; }

    public String getUpdatedBy() { return updatedBy; }
    public void setUpdatedBy(String updatedBy) { this.updatedBy = updatedBy; }
}
