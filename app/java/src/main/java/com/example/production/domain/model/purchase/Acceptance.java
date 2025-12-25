package com.example.production.domain.model.purchase;

import lombok.*;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;

/**
 * 検収データ
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class Acceptance {
    private Integer id;
    private String acceptanceNumber;
    private String inspectionNumber;
    private String purchaseOrderNumber;
    private Integer lineNumber;
    private LocalDate acceptanceDate;
    private String acceptorCode;
    private String supplierCode;
    private String itemCode;
    private Boolean miscellaneousItemFlag;
    private BigDecimal acceptedQuantity;
    private BigDecimal unitPrice;
    private BigDecimal amount;
    private BigDecimal taxAmount;
    private String remarks;
    private LocalDateTime createdAt;
    private String createdBy;
    private LocalDateTime updatedAt;
    private String updatedBy;
}
