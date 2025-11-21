package com.example.accounting.domain.model;

/**
 * 勘定科目ドメインモデル
 */
public class Account {
    private String accountCode;      // 勘定科目コード
    private String accountName;      // 勘定科目名
    private String accountAbbr;      // 勘定科目略名
    private String accountKana;      // 勘定科目カナ
    private String bsplType;         // BSPL区分
    private String debitCreditType;  // 貸借区分
    private String elementType;      // 取引要素区分
    private String aggregationType;  // 集計区分
    private Integer displayOrder;    // 表示順

    public Account() {
    }

    @SuppressWarnings("checkstyle:ParameterNumber")
    public Account(String accountCode, String accountName, String accountAbbr,
                   String accountKana, String bsplType, String debitCreditType,
                   String elementType, String aggregationType, Integer displayOrder) {
        this.accountCode = accountCode;
        this.accountName = accountName;
        this.accountAbbr = accountAbbr;
        this.accountKana = accountKana;
        this.bsplType = bsplType;
        this.debitCreditType = debitCreditType;
        this.elementType = elementType;
        this.aggregationType = aggregationType;
        this.displayOrder = displayOrder;
    }

    /**
     * 貸借対照表科目かどうか
     */
    public boolean isBalanceSheetAccount() {
        return "B".equals(bsplType);
    }

    /**
     * 損益計算書科目かどうか
     */
    public boolean isProfitLossAccount() {
        return "P".equals(bsplType);
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

    public String getAggregationType() {
        return aggregationType;
    }

    public void setAggregationType(String aggregationType) {
        this.aggregationType = aggregationType;
    }

    public Integer getDisplayOrder() {
        return displayOrder;
    }

    public void setDisplayOrder(Integer displayOrder) {
        this.displayOrder = displayOrder;
    }
}
