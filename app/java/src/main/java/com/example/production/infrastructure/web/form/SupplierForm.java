package com.example.production.infrastructure.web.form;

import com.example.production.domain.model.supplier.Supplier;
import com.example.production.domain.model.supplier.SupplierType;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Pattern;
import jakarta.validation.constraints.Size;
import lombok.Data;

import java.time.LocalDate;

/**
 * 取引先登録・編集フォーム
 */
@Data
public class SupplierForm {

    @NotBlank(message = "取引先コードは必須です")
    @Size(max = 20, message = "取引先コードは20文字以内で入力してください")
    private String supplierCode;

    @NotBlank(message = "取引先名は必須です")
    @Size(max = 100, message = "取引先名は100文字以内で入力してください")
    private String supplierName;

    @Size(max = 100, message = "取引先名カナは100文字以内で入力してください")
    private String supplierNameKana;

    @NotNull(message = "取引先区分は必須です")
    private SupplierType supplierType;

    @Pattern(regexp = "^$|^\\d{3}-\\d{4}$", message = "郵便番号は「000-0000」形式で入力してください")
    private String postalCode;

    @Size(max = 200, message = "住所は200文字以内で入力してください")
    private String address;

    @Size(max = 20, message = "電話番号は20文字以内で入力してください")
    private String phoneNumber;

    @Size(max = 20, message = "FAX番号は20文字以内で入力してください")
    private String faxNumber;

    @Size(max = 50, message = "担当者名は50文字以内で入力してください")
    private String contactPerson;

    /**
     * フォームをエンティティに変換（新規登録用）
     */
    public Supplier toEntity() {
        return Supplier.builder()
                .supplierCode(this.supplierCode)
                .supplierName(this.supplierName)
                .supplierNameKana(this.supplierNameKana)
                .supplierType(this.supplierType)
                .postalCode(this.postalCode)
                .address(this.address)
                .phoneNumber(this.phoneNumber)
                .faxNumber(this.faxNumber)
                .contactPerson(this.contactPerson)
                .effectiveFrom(LocalDate.now())
                .build();
    }

    /**
     * エンティティからフォームを生成
     */
    public static SupplierForm from(Supplier supplier) {
        SupplierForm form = new SupplierForm();
        form.setSupplierCode(supplier.getSupplierCode());
        form.setSupplierName(supplier.getSupplierName());
        form.setSupplierNameKana(supplier.getSupplierNameKana());
        form.setSupplierType(supplier.getSupplierType());
        form.setPostalCode(supplier.getPostalCode());
        form.setAddress(supplier.getAddress());
        form.setPhoneNumber(supplier.getPhoneNumber());
        form.setFaxNumber(supplier.getFaxNumber());
        form.setContactPerson(supplier.getContactPerson());
        return form;
    }
}
