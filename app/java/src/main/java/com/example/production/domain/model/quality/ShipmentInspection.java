package com.example.production.domain.model.quality;

import lombok.*;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;

/**
 * 出荷検査データ
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ShipmentInspection {
    private Integer id;
    private String shipmentInspectionNumber;
    private String shipmentNumber;
    private String itemCode;
    private LocalDate inspectionDate;
    private String inspectorCode;
    private BigDecimal inspectionQuantity;
    private BigDecimal passedQuantity;
    private BigDecimal failedQuantity;
    private InspectionJudgment judgment;
    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;

    private List<ShipmentInspectionResult> results;
}
