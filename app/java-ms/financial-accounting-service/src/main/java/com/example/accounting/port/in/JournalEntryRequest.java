package com.example.accounting.port.in;

import java.math.BigDecimal;

public class JournalEntryRequest {
    private String accountCode;
    private BigDecimal debitAmount;
    private BigDecimal creditAmount;
    private String description;

    public JournalEntryRequest() {
    }

    public JournalEntryRequest(String accountCode, BigDecimal debitAmount, BigDecimal creditAmount, String description) {
        this.accountCode = accountCode;
        this.debitAmount = debitAmount;
        this.creditAmount = creditAmount;
        this.description = description;
    }

    public String getAccountCode() {
        return accountCode;
    }

    public void setAccountCode(String accountCode) {
        this.accountCode = accountCode;
    }

    public BigDecimal getDebitAmount() {
        return debitAmount;
    }

    public void setDebitAmount(BigDecimal debitAmount) {
        this.debitAmount = debitAmount;
    }

    public BigDecimal getCreditAmount() {
        return creditAmount;
    }

    public void setCreditAmount(BigDecimal creditAmount) {
        this.creditAmount = creditAmount;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }
}
