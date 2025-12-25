package com.example.production.domain.model.subcontract;

import lombok.*;
import java.math.BigDecimal;
import java.time.LocalDateTime;

/**
 * 支給明細データ
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class SupplyDetail {
    private Integer id;
    private String supplyNumber;
    private Integer lineNumber;
    private String itemCode;
    private BigDecimal quantity;
    private BigDecimal unitPrice;
    private BigDecimal amount;
    private String remarks;
    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;
}
