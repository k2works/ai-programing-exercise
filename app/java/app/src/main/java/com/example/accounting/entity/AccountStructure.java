package com.example.accounting.entity;

import java.time.LocalDateTime;
import java.util.Objects;

/**
 * 勘定科目構成エンティティクラス
 * データベースの日本語カラム名と英語プロパティ名をマッピング
 */
public class AccountStructure {
    private String accountCode;           // 勘定科目コード
    private String accountPath;            // 勘定科目パス
    private Integer hierarchyLevel;        // 階層レベル
    private String parentAccountCode;      // 親科目コード
    private Integer displayOrder;          // 表示順序
    private LocalDateTime createdAt;       // 作成日時
    private LocalDateTime updatedAt;       // 更新日時

    public AccountStructure() {
    }

    public AccountStructure(String accountCode, String accountPath, Integer hierarchyLevel,
                           String parentAccountCode, Integer displayOrder) {
        this.accountCode = accountCode;
        this.accountPath = accountPath;
        this.hierarchyLevel = hierarchyLevel;
        this.parentAccountCode = parentAccountCode;
        this.displayOrder = displayOrder;
    }

    // Getters
    public String getAccountCode() { return accountCode; }
    public String getAccountPath() { return accountPath; }
    public Integer getHierarchyLevel() { return hierarchyLevel; }
    public String getParentAccountCode() { return parentAccountCode; }
    public Integer getDisplayOrder() { return displayOrder; }
    public LocalDateTime getCreatedAt() { return createdAt; }
    public LocalDateTime getUpdatedAt() { return updatedAt; }

    // Setters
    public void setAccountCode(String accountCode) { this.accountCode = accountCode; }
    public void setAccountPath(String accountPath) { this.accountPath = accountPath; }
    public void setHierarchyLevel(Integer hierarchyLevel) { this.hierarchyLevel = hierarchyLevel; }
    public void setParentAccountCode(String parentAccountCode) { this.parentAccountCode = parentAccountCode; }
    public void setDisplayOrder(Integer displayOrder) { this.displayOrder = displayOrder; }
    public void setCreatedAt(LocalDateTime createdAt) { this.createdAt = createdAt; }
    public void setUpdatedAt(LocalDateTime updatedAt) { this.updatedAt = updatedAt; }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        AccountStructure that = (AccountStructure) o;
        return Objects.equals(accountCode, that.accountCode);
    }

    @Override
    public int hashCode() {
        return Objects.hash(accountCode);
    }

    @Override
    public String toString() {
        return "AccountStructure{"
                + "accountCode='" + accountCode + '\''
                + ", accountPath='" + accountPath + '\''
                + ", hierarchyLevel=" + hierarchyLevel
                + ", parentAccountCode='" + parentAccountCode + '\''
                + ", displayOrder=" + displayOrder
                + '}';
    }
}
