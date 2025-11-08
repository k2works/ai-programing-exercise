package com.example.sales.domain.model;

import java.time.LocalDateTime;

/**
 * 商品マスタのEntityクラス
 */
public class Product {
    private String productCode;
    private String productFormalName;
    private String productAbbreviation;
    private String productNameKana;
    private String productType;
    private String modelNumber;
    private Integer sellingPrice;
    private Integer purchasePrice;
    private Integer costOfSales;
    private Integer taxType;
    private String productCategoryCode;
    private Integer miscellaneousType;
    private Integer inventoryManagementFlag;
    private Integer inventoryAllocationFlag;
    private String supplierCode;
    private Integer supplierBranch;
    private LocalDateTime createdAt;
    private String createdBy;
    private LocalDateTime updatedAt;
    private String updatedBy;

    // デフォルトコンストラクタ
    public Product() {}

    // Getter/Setter（省略 - 全フィールドに対して作成）
    public String getProductCode() { return productCode; }
    public void setProductCode(String productCode) { this.productCode = productCode; }

    public String getProductFormalName() { return productFormalName; }
    public void setProductFormalName(String productFormalName) {
        this.productFormalName = productFormalName;
    }

    public String getProductAbbreviation() { return productAbbreviation; }
    public void setProductAbbreviation(String productAbbreviation) {
        this.productAbbreviation = productAbbreviation;
    }

    public String getProductNameKana() { return productNameKana; }
    public void setProductNameKana(String productNameKana) {
        this.productNameKana = productNameKana;
    }

    public String getProductType() { return productType; }
    public void setProductType(String productType) { this.productType = productType; }

    public String getModelNumber() { return modelNumber; }
    public void setModelNumber(String modelNumber) { this.modelNumber = modelNumber; }

    public Integer getSellingPrice() { return sellingPrice; }
    public void setSellingPrice(Integer sellingPrice) { this.sellingPrice = sellingPrice; }

    public Integer getPurchasePrice() { return purchasePrice; }
    public void setPurchasePrice(Integer purchasePrice) { this.purchasePrice = purchasePrice; }

    public Integer getCostOfSales() { return costOfSales; }
    public void setCostOfSales(Integer costOfSales) { this.costOfSales = costOfSales; }

    public Integer getTaxType() { return taxType; }
    public void setTaxType(Integer taxType) { this.taxType = taxType; }

    public String getProductCategoryCode() { return productCategoryCode; }
    public void setProductCategoryCode(String productCategoryCode) {
        this.productCategoryCode = productCategoryCode;
    }

    public Integer getMiscellaneousType() { return miscellaneousType; }
    public void setMiscellaneousType(Integer miscellaneousType) {
        this.miscellaneousType = miscellaneousType;
    }

    public Integer getInventoryManagementFlag() { return inventoryManagementFlag; }
    public void setInventoryManagementFlag(Integer inventoryManagementFlag) {
        this.inventoryManagementFlag = inventoryManagementFlag;
    }

    public Integer getInventoryAllocationFlag() { return inventoryAllocationFlag; }
    public void setInventoryAllocationFlag(Integer inventoryAllocationFlag) {
        this.inventoryAllocationFlag = inventoryAllocationFlag;
    }

    public String getSupplierCode() { return supplierCode; }
    public void setSupplierCode(String supplierCode) { this.supplierCode = supplierCode; }

    public Integer getSupplierBranch() { return supplierBranch; }
    public void setSupplierBranch(Integer supplierBranch) {
        this.supplierBranch = supplierBranch;
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
