package com.example.production.application.port.in.command;

import lombok.Builder;
import lombok.Data;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;

/**
 * 発注作成コマンド
 */
@Data
@Builder
public class CreatePurchaseOrderCommand {
    private String supplierCode;
    private String ordererCode;
    private String departmentCode;
    private String remarks;
    private List<PurchaseOrderDetailCommand> details;

    @Data
    @Builder
    public static class PurchaseOrderDetailCommand {
        private String itemCode;
        private BigDecimal orderQuantity;
        private BigDecimal unitPrice;
        private LocalDate deliveryDate;
    }
}
