package com.example.production.domain.model.cost;

import lombok.*;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;

/**
 * 標準原価マスタ
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class StandardCost {
    private Integer id;
    private String itemCode;
    private LocalDate effectiveStartDate;
    private LocalDate effectiveEndDate;
    private BigDecimal standardMaterialCost;
    private BigDecimal standardLaborCost;
    private BigDecimal standardExpense;
    private BigDecimal standardManufacturingCost;
    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;
}
