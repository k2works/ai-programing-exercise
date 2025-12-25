package com.example.production.domain.model.quality;

import lombok.*;
import java.math.BigDecimal;
import java.time.LocalDateTime;

/**
 * ロット構成（トレーサビリティ用）
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class LotComposition {
    private Integer id;
    private String parentLotNumber;
    private String childLotNumber;
    private BigDecimal usedQuantity;
    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;
}
