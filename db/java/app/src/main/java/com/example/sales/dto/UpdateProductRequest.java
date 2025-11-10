package com.example.sales.dto;

import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.Size;
import lombok.Data;

/**
 * 商品更新リクエストDTO
 * すべてのフィールドはオプション
 */
@Data
public class UpdateProductRequest {

    @Size(max = 40, message = "商品正式名は40文字以内で入力してください")
    private String fullName;

    @Size(max = 10, message = "商品名は10文字以内で入力してください")
    private String name;

    @Size(max = 20, message = "商品カナ名は20文字以内で入力してください")
    private String kanaName;

    @Min(value = 0, message = "販売単価は0以上で入力してください")
    private Integer unitPrice;

    @Min(value = 0, message = "売上原価は0以上で入力してください")
    private Integer primeCost;

    @Size(max = 8, message = "仕入先コードは8文字以内で入力してください")
    private String supplierCode;
}
