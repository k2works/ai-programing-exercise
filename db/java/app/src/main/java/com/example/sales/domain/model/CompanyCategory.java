package com.example.sales.domain.model;

import java.time.LocalDateTime;

/**
 * 取引先分類マスタのEntityクラス
 */
public class CompanyCategory {
    private String categoryTypeCode;
    private String companyCategoryCode;
    private String companyCategoryName;
    private LocalDateTime createdAt;
    private String createdBy;
    private LocalDateTime updatedAt;
    private String updatedBy;

    // デフォルトコンストラクタ
    public CompanyCategory() {}

    // Getter/Setter
    public String getCategoryTypeCode() { return categoryTypeCode; }
    public void setCategoryTypeCode(String categoryTypeCode) {
        this.categoryTypeCode = categoryTypeCode;
    }

    public String getCompanyCategoryCode() { return companyCategoryCode; }
    public void setCompanyCategoryCode(String companyCategoryCode) {
        this.companyCategoryCode = companyCategoryCode;
    }

    public String getCompanyCategoryName() { return companyCategoryName; }
    public void setCompanyCategoryName(String companyCategoryName) {
        this.companyCategoryName = companyCategoryName;
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
