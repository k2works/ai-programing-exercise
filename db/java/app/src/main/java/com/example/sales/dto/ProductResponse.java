package com.example.sales.dto;

import com.example.sales.domain.model.Product;
import lombok.Data;

import java.time.LocalDateTime;

/**
 * 商品レスポンスDTO
 */
@Data
public class ProductResponse {

    private String productCode;
    private String fullName;
    private String name;
    private String kanaName;
    private Integer unitPrice;
    private Integer primeCost;
    private String supplierCode;
    private LocalDateTime createdAt;
    private String createdBy;
    private LocalDateTime updatedAt;
    private String updatedBy;

    /**
     * EntityからDTOへの変換
     */
    public static ProductResponse fromEntity(Product product) {
        ProductResponse response = new ProductResponse();
        response.setProductCode(product.getProductCode());
        response.setFullName(product.getProductFormalName());
        response.setName(product.getProductAbbreviation());
        response.setKanaName(product.getProductNameKana());
        response.setUnitPrice(product.getSellingPrice());
        response.setPrimeCost(product.getCostOfSales());
        response.setSupplierCode(product.getSupplierCode());
        response.setCreatedAt(product.getCreatedAt());
        response.setCreatedBy(product.getCreatedBy());
        response.setUpdatedAt(product.getUpdatedAt());
        response.setUpdatedBy(product.getUpdatedBy());
        return response;
    }
}
