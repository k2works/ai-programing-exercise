package com.example.production.domain.model.subcontract;

import lombok.*;

import java.math.BigDecimal;
import java.time.LocalDateTime;

/**
 * 消費明細データ
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ConsumptionDetail {
    private Integer id;
    private String consumptionNumber;
    private Integer lineNumber;
    private String itemCode;
    private BigDecimal quantity;
    private String remarks;
    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;
}
