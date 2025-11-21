package com.example.accounting.domain;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.Objects;

/**
 * 課税取引エンティティクラス
 * データベースの日本語カラム名と英語プロパティ名をマッピング
 */
public class TaxTransaction {
    private String taxCode;              // 課税取引コード
    private String taxName;              // 課税取引名
    private BigDecimal taxRate;          // 税率
    private String description;          // 説明
    private Boolean isActive;            // 有効フラグ
    private LocalDateTime createdAt;     // 作成日時
    private LocalDateTime updatedAt;     // 更新日時

    public TaxTransaction() {
    }

    public TaxTransaction(String taxCode, String taxName, BigDecimal taxRate, String description) {
        this.taxCode = taxCode;
        this.taxName = taxName;
        this.taxRate = taxRate;
        this.description = description;
        this.isActive = true;
    }

    // Getters
    public String getTaxCode() {
        return taxCode;
    }

    public String getTaxName() {
        return taxName;
    }

    public BigDecimal getTaxRate() {
        return taxRate;
    }

    public String getDescription() {
        return description;
    }

    public Boolean getIsActive() {
        return isActive;
    }

    public LocalDateTime getCreatedAt() {
        return createdAt;
    }

    public LocalDateTime getUpdatedAt() {
        return updatedAt;
    }

    // Setters
    public void setTaxCode(String taxCode) {
        this.taxCode = taxCode;
    }

    public void setTaxName(String taxName) {
        this.taxName = taxName;
    }

    public void setTaxRate(BigDecimal taxRate) {
        this.taxRate = taxRate;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public void setIsActive(Boolean isActive) {
        this.isActive = isActive;
    }

    public void setCreatedAt(LocalDateTime createdAt) {
        this.createdAt = createdAt;
    }

    public void setUpdatedAt(LocalDateTime updatedAt) {
        this.updatedAt = updatedAt;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        TaxTransaction that = (TaxTransaction) o;
        return Objects.equals(taxCode, that.taxCode);
    }

    @Override
    public int hashCode() {
        return Objects.hash(taxCode);
    }

    @Override
    public String toString() {
        return "TaxTransaction{"
                + "taxCode='" + taxCode + '\''
                + ", taxName='" + taxName + '\''
                + ", taxRate=" + taxRate
                + ", isActive=" + isActive
                + '}';
    }
}
