package com.example.sales.domain.model;

import java.time.LocalDateTime;

/**
 * 取引先分類所属マスタのEntityクラス
 */
public class CompanyCategoryGroup {
    private String categoryTypeCode;
    private String companyCategoryCode;
    private String companyCode;
    private LocalDateTime createdAt;
    private String createdBy;
    private LocalDateTime updatedAt;
    private String updatedBy;

    // デフォルトコンストラクタ
    public CompanyCategoryGroup() {}

    // Getter/Setter
    public String getCategoryTypeCode() { return categoryTypeCode; }
    public void setCategoryTypeCode(String categoryTypeCode) {
        this.categoryTypeCode = categoryTypeCode;
    }

    public String getCompanyCategoryCode() { return companyCategoryCode; }
    public void setCompanyCategoryCode(String companyCategoryCode) {
        this.companyCategoryCode = companyCategoryCode;
    }

    public String getCompanyCode() { return companyCode; }
    public void setCompanyCode(String companyCode) { this.companyCode = companyCode; }

    public LocalDateTime getCreatedAt() { return createdAt; }
    public void setCreatedAt(LocalDateTime createdAt) { this.createdAt = createdAt; }

    public String getCreatedBy() { return createdBy; }
    public void setCreatedBy(String createdBy) { this.createdBy = createdBy; }

    public LocalDateTime getUpdatedAt() { return updatedAt; }
    public void setUpdatedAt(LocalDateTime updatedAt) { this.updatedAt = updatedAt; }

    public String getUpdatedBy() { return updatedBy; }
    public void setUpdatedBy(String updatedBy) { this.updatedBy = updatedBy; }
}
