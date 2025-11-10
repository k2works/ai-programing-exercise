package com.example.sales.dto;

import jakarta.validation.constraints.*;
import lombok.Data;

/**
 * 商品作成リクエストDTO
 */
@Data
public class CreateProductRequest {

    @NotBlank(message = "商品コードは必須です")
    @Size(max = 16, message = "商品コードは16文字以内で入力してください")
    private String productCode;

    @NotBlank(message = "商品正式名は必須です")
    @Size(max = 40, message = "商品正式名は40文字以内で入力してください")
    private String fullName;

    @NotBlank(message = "商品名は必須です")
    @Size(max = 10, message = "商品名は10文字以内で入力してください")
    private String name;

    @NotBlank(message = "商品カナ名は必須です")
    @Size(max = 20, message = "商品カナ名は20文字以内で入力してください")
    private String kanaName;

    @NotNull(message = "販売単価は必須です")
    @Min(value = 0, message = "販売単価は0以上で入力してください")
    private Integer unitPrice;

    @NotNull(message = "売上原価は必須です")
    @Min(value = 0, message = "売上原価は0以上で入力してください")
    private Integer primeCost;

    @NotBlank(message = "仕入先コードは必須です")
    @Size(max = 8, message = "仕入先コードは8文字以内で入力してください")
    private String supplierCode;
}
