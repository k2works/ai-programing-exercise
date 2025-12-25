package com.example.production.domain.model.purchase;

import lombok.*;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;

/**
 * 入荷受入データ
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class Receiving {
    private Integer id;
    private String receivingNumber;
    private String purchaseOrderNumber;
    private Integer lineNumber;
    private LocalDate receivingDate;
    private String receiverCode;
    private ReceivingType receivingType;
    private String itemCode;
    private Boolean miscellaneousItemFlag;
    private BigDecimal receivingQuantity;
    private String remarks;
    private LocalDateTime createdAt;
    private String createdBy;
    private LocalDateTime updatedAt;
    private String updatedBy;
}
