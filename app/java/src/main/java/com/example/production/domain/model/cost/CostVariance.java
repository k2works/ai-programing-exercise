package com.example.production.domain.model.cost;

import lombok.*;
import java.math.BigDecimal;
import java.time.LocalDateTime;

/**
 * 原価差異データ
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class CostVariance {
    private Integer id;
    private String workOrderNumber;
    private String itemCode;
    private BigDecimal materialCostVariance;
    private BigDecimal laborCostVariance;
    private BigDecimal expenseVariance;
    private BigDecimal totalVariance;
    private LocalDateTime createdAt;
}
