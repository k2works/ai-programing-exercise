package com.example.production.domain.model.purchase;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class PurchaseOrderDetail {
    private Integer id;
    private String purchaseOrderNumber;
    private Integer lineNumber;
    private String orderNumber;
    private String deliveryLocationCode;
    private String itemCode;
    @Builder.Default
    private Boolean miscellaneousItemFlag = false;
    private LocalDate expectedReceivingDate;
    private LocalDate confirmedDeliveryDate;
    private BigDecimal orderUnitPrice;
    private BigDecimal orderQuantity;
    @Builder.Default
    private BigDecimal receivedQuantity = BigDecimal.ZERO;
    @Builder.Default
    private BigDecimal inspectedQuantity = BigDecimal.ZERO;
    @Builder.Default
    private BigDecimal acceptedQuantity = BigDecimal.ZERO;
    private BigDecimal orderAmount;
    @Builder.Default
    private BigDecimal taxAmount = BigDecimal.ZERO;
    @Builder.Default
    private Boolean completedFlag = false;
    private String detailRemarks;
    private LocalDateTime createdAt;
    private String createdBy;
    private LocalDateTime updatedAt;
    private String updatedBy;
}
