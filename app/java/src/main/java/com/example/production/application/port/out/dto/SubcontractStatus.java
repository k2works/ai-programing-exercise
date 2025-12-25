package com.example.production.application.port.out.dto;

import com.example.production.domain.model.purchase.PurchaseOrderStatus;
import lombok.Builder;
import lombok.Data;

import java.math.BigDecimal;

/**
 * 外注委託状況DTO
 */
@Data
@Builder
public class SubcontractStatus {
    private String purchaseOrderNumber;
    private PurchaseOrderStatus status;
    private BigDecimal suppliedQuantity;
    private BigDecimal consumedQuantity;
    private BigDecimal acceptedQuantity;
    private BigDecimal yieldRate;
}
