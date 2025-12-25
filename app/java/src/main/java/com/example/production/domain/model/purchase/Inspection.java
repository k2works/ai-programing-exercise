package com.example.production.domain.model.purchase;

import lombok.*;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;

/**
 * 受入検査データ
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class Inspection {
    private Integer id;
    private String inspectionNumber;
    private String receivingNumber;
    private String purchaseOrderNumber;
    private Integer lineNumber;
    private LocalDate inspectionDate;
    private String inspectorCode;
    private String itemCode;
    private Boolean miscellaneousItemFlag;
    private BigDecimal goodQuantity;
    private BigDecimal defectQuantity;
    private String remarks;
    private LocalDateTime createdAt;
    private String createdBy;
    private LocalDateTime updatedAt;
    private String updatedBy;
}
