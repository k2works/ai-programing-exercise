package com.example.production.infrastructure.web.dto;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import lombok.Data;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;

/**
 * 発注作成リクエスト
 */
@Data
public class CreatePurchaseOrderRequest {

    @NotBlank(message = "取引先コードは必須です")
    private String supplierCode;

    private String ordererCode;
    private String departmentCode;
    private String remarks;

    @NotNull(message = "明細は必須です")
    private List<PurchaseOrderDetailRequest> details;

    @Data
    public static class PurchaseOrderDetailRequest {
        @NotBlank(message = "品目コードは必須です")
        private String itemCode;

        @NotNull(message = "発注数量は必須です")
        private BigDecimal orderQuantity;

        private BigDecimal unitPrice;

        @NotNull(message = "納期は必須です")
        private LocalDate deliveryDate;
    }
}
