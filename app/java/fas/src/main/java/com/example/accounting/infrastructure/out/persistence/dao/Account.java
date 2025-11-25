package com.example.accounting.infrastructure.out.persistence.dao;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.Objects;

/**
 * 勘定科目エンティティクラス
 * データベースの日本語カラム名と英語プロパティ名をマッピング
 */
public class Account {
    private Integer accountId;           // 勘定科目ID
    private String accountCode;          // 勘定科目コード
    private String accountName;          // 勘定科目名
    private String accountNameKana;      // 勘定科目カナ
    private String accountType;          // 勘定科目種別
    private Boolean isSummaryAccount;    // 合計科目
    private String bsplType;             // BSPL区分
    private String transactionElementType; // 取引要素区分
    private String expenseType;          // 費用区分
    private Integer displayOrder;        // 表示順序
    private Boolean isAggregationTarget; // 集計対象
    private String taxCode;              // 課税取引コード
    private BigDecimal balance;          // 残高
    private LocalDateTime createdAt;     // 作成日時
    private LocalDateTime updatedAt;     // 更新日時

    public Account() {
    }

    public Account(String accountCode, String accountName, String accountType, Boolean isSummaryAccount) {
        this.accountCode = accountCode;
        this.accountName = accountName;
        this.accountType = accountType;
        this.isSummaryAccount = isSummaryAccount;
        this.isAggregationTarget = true;
        this.balance = BigDecimal.ZERO;
    }

    // Getters
    public Integer getAccountId() {
        return accountId;
    }

    public String getAccountCode() {
        return accountCode;
    }

    public String getAccountName() {
        return accountName;
    }

    public String getAccountNameKana() {
        return accountNameKana;
    }

    public String getAccountType() {
        return accountType;
    }

    public Boolean getIsSummaryAccount() {
        return isSummaryAccount;
    }

    public String getBsplType() {
        return bsplType;
    }

    public String getTransactionElementType() {
        return transactionElementType;
    }

    public String getExpenseType() {
        return expenseType;
    }

    public Integer getDisplayOrder() {
        return displayOrder;
    }

    public Boolean getIsAggregationTarget() {
        return isAggregationTarget;
    }

    public String getTaxCode() {
        return taxCode;
    }

    public BigDecimal getBalance() {
        return balance;
    }

    public LocalDateTime getCreatedAt() {
        return createdAt;
    }

    public LocalDateTime getUpdatedAt() {
        return updatedAt;
    }

    // Setters
    public void setAccountId(Integer accountId) {
        this.accountId = accountId;
    }

    public void setAccountCode(String accountCode) {
        this.accountCode = accountCode;
    }

    public void setAccountName(String accountName) {
        this.accountName = accountName;
    }

    public void setAccountNameKana(String accountNameKana) {
        this.accountNameKana = accountNameKana;
    }

    public void setAccountType(String accountType) {
        this.accountType = accountType;
    }

    public void setIsSummaryAccount(Boolean isSummaryAccount) {
        this.isSummaryAccount = isSummaryAccount;
    }

    public void setBsplType(String bsplType) {
        this.bsplType = bsplType;
    }

    public void setTransactionElementType(String transactionElementType) {
        this.transactionElementType = transactionElementType;
    }

    public void setExpenseType(String expenseType) {
        this.expenseType = expenseType;
    }

    public void setDisplayOrder(Integer displayOrder) {
        this.displayOrder = displayOrder;
    }

    public void setIsAggregationTarget(Boolean isAggregationTarget) {
        this.isAggregationTarget = isAggregationTarget;
    }

    public void setTaxCode(String taxCode) {
        this.taxCode = taxCode;
    }

    public void setBalance(BigDecimal balance) {
        this.balance = balance;
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
        Account account = (Account) o;
        return Objects.equals(accountCode, account.accountCode);
    }

    @Override
    public int hashCode() {
        return Objects.hash(accountCode);
    }

    @Override
    public String toString() {
        return "Account{"
                + "accountCode='" + accountCode + '\''
                + ", accountName='" + accountName + '\''
                + ", accountType='" + accountType + '\''
                + ", isSummaryAccount=" + isSummaryAccount
                + ", balance=" + balance
                + '}';
    }
}
