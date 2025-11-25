package com.example.accounting.domain.model.financial;

import java.math.BigDecimal;

/**
 * 損益計算書の項目（勘定科目ごとの金額と構成比率）
 */
public class IncomeStatementItem {
    private String accountCode;
    private String accountName;
    private BigDecimal balance;
    private BigDecimal percentage;  // 対売上比

    public IncomeStatementItem() {
    }

    public IncomeStatementItem(String accountCode, String accountName,
                               BigDecimal balance, BigDecimal percentage) {
        this.accountCode = accountCode;
        this.accountName = accountName;
        this.balance = balance;
        this.percentage = percentage;
    }

    public String getAccountCode() {
        return accountCode;
    }

    public void setAccountCode(String accountCode) {
        this.accountCode = accountCode;
    }

    public String getAccountName() {
        return accountName;
    }

    public void setAccountName(String accountName) {
        this.accountName = accountName;
    }

    public BigDecimal getBalance() {
        return balance;
    }

    public void setBalance(BigDecimal balance) {
        this.balance = balance;
    }

    public BigDecimal getPercentage() {
        return percentage;
    }

    public void setPercentage(BigDecimal percentage) {
        this.percentage = percentage;
    }
}
