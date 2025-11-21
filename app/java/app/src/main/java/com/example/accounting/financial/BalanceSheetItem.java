package com.example.accounting.financial;

import java.math.BigDecimal;

/**
 * 貸借対照表の明細項目
 */
public class BalanceSheetItem {
    private String accountCode;
    private String accountName;
    private BigDecimal balance;
    private BigDecimal ratio;  // 構成比率（%）

    public BalanceSheetItem(String accountCode, String accountName, BigDecimal balance, BigDecimal ratio) {
        this.accountCode = accountCode;
        this.accountName = accountName;
        this.balance = balance;
        this.ratio = ratio;
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

    public BigDecimal getRatio() {
        return ratio;
    }

    public void setRatio(BigDecimal ratio) {
        this.ratio = ratio;
    }
}
