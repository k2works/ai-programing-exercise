package com.example.accounting.infrastructure.in.web.dto;

import com.example.accounting.domain.model.Account;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Pattern;
import jakarta.validation.constraints.Size;

/**
 * 勘定科目リクエスト DTO
 */
public class AccountRequest {

    @NotBlank(message = "勘定科目コードは必須です")
    @Size(max = 10, message = "勘定科目コードは10文字以内である必要があります")
    private String accountCode;

    @NotBlank(message = "勘定科目名は必須です")
    @Size(max = 40, message = "勘定科目名は40文字以内である必要があります")
    private String accountName;

    @Size(max = 20, message = "勘定科目略名は20文字以内である必要があります")
    private String accountAbbr;

    @Size(max = 40, message = "勘定科目カナは40文字以内である必要があります")
    private String accountKana;

    @NotBlank(message = "BSPL区分は必須です")
    @Pattern(regexp = "^[BP]$", message = "BSPL区分は 'B' または 'P' である必要があります")
    private String bsplType;

    @Pattern(regexp = "^[借貸]$", message = "貸借区分は '借' または '貸' である必要があります")
    private String debitCreditType;

    @Size(max = 10, message = "取引要素区分は10文字以内である必要があります")
    private String elementType;

    private Integer displayOrder;

    /**
     * Domain Modelへの変換
     */
    public Account toDomain() {
        return new Account(
                accountCode,
                accountName,
                accountAbbr,
                accountKana,
                bsplType,
                debitCreditType,
                elementType,
                "1", // デフォルトで集計対象
                displayOrder
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
