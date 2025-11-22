package com.example.accounting.infrastructure.in.web.dto.financial;

import com.example.accounting.domain.model.financial.Account;

/**
 * 勘定科目レスポンス DTO
 */
public class AccountResponse {
    private String accountCode;
    private String accountName;
    private String accountAbbr;
    private String accountKana;
    private String bsplType;
    private String debitCreditType;
    private String elementType;
    private Integer displayOrder;

    public AccountResponse() {
    }

    public AccountResponse(String accountCode, String accountName, String accountAbbr,
                           String accountKana, String bsplType, String debitCreditType,
                           String elementType, Integer displayOrder) {
        this.accountCode = accountCode;
        this.accountName = accountName;
        this.accountAbbr = accountAbbr;
        this.accountKana = accountKana;
        this.bsplType = bsplType;
        this.debitCreditType = debitCreditType;
        this.elementType = elementType;
        this.displayOrder = displayOrder;
    }

    /**
     * Domain Modelからの変換
     */
    public static AccountResponse from(Account account) {
        return new AccountResponse(
                account.getAccountCode(),
                account.getAccountName(),
                account.getAccountAbbr(),
                account.getAccountKana(),
                account.getBsplType(),
                account.getDebitCreditType(),
                account.getElementType(),
                account.getDisplayOrder()
        );
    }

    // Getters and Setters
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

    public String getAccountAbbr() {
        return accountAbbr;
    }

    public void setAccountAbbr(String accountAbbr) {
        this.accountAbbr = accountAbbr;
    }

    public String getAccountKana() {
        return accountKana;
    }

    public void setAccountKana(String accountKana) {
        this.accountKana = accountKana;
    }

    public String getBsplType() {
        return bsplType;
    }

    public void setBsplType(String bsplType) {
        this.bsplType = bsplType;
    }

    public String getDebitCreditType() {
        return debitCreditType;
    }

    public void setDebitCreditType(String debitCreditType) {
        this.debitCreditType = debitCreditType;
    }

    public String getElementType() {
        return elementType;
    }

    public void setElementType(String elementType) {
        this.elementType = elementType;
    }

    public Integer getDisplayOrder() {
        return displayOrder;
    }

    public void setDisplayOrder(Integer displayOrder) {
        this.displayOrder = displayOrder;
    }
}
