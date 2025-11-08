package com.example.sales.domain.model;

import java.time.LocalDateTime;

/**
 * 商品分類マスタのEntityクラス
 */
public class ProductCategory {
    private String productCategoryCode;
    private String productCategoryName;
    private Integer productCategoryLevel;
    private String productCategoryPath;
    private Integer lowestLevelFlag;
    private LocalDateTime createdAt;
    private String createdBy;
    private LocalDateTime updatedAt;
    private String updatedBy;

    // デフォルトコンストラクタ
    public ProductCategory() {}

    // Getter/Setter
    public String getProductCategoryCode() { return productCategoryCode; }
    public void setProductCategoryCode(String productCategoryCode) {
        this.productCategoryCode = productCategoryCode;
    }

    public String getProductCategoryName() { return productCategoryName; }
    public void setProductCategoryName(String productCategoryName) {
        this.productCategoryName = productCategoryName;
    }

    public Integer getProductCategoryLevel() { return productCategoryLevel; }
    public void setProductCategoryLevel(Integer productCategoryLevel) {
        this.productCategoryLevel = productCategoryLevel;
    }

    public String getProductCategoryPath() { return productCategoryPath; }
    public void setProductCategoryPath(String productCategoryPath) {
        this.productCategoryPath = productCategoryPath;
    }

    public Integer getLowestLevelFlag() { return lowestLevelFlag; }
    public void setLowestLevelFlag(Integer lowestLevelFlag) {
        this.lowestLevelFlag = lowestLevelFlag;
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
