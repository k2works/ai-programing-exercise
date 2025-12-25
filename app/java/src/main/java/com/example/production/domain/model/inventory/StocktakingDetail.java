package com.example.production.domain.model.inventory;

import lombok.*;
import java.math.BigDecimal;
import java.time.LocalDateTime;

/**
 * 棚卸明細データエンティティ
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class StocktakingDetail {
    private Long id;
    private String stocktakingNumber;
    private Integer lineNumber;
    private String itemCode;
    private BigDecimal bookQuantity;
    private BigDecimal actualQuantity;
    private BigDecimal differenceQuantity;
    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;
}
