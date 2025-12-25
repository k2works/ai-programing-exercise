package com.example.production.domain.model.inventory;

import lombok.*;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;

/**
 * 在庫調整データエンティティ
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class StockAdjustment {
    private Long id;
    private String adjustmentNumber;
    private String stocktakingNumber;
    private String itemCode;
    private String locationCode;
    private LocalDate adjustmentDate;
    private String adjusterCode;
    private BigDecimal adjustmentQuantity;
    private String reasonCode;
    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;
}
