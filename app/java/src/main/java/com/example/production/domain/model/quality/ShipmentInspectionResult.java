package com.example.production.domain.model.quality;

import lombok.*;
import java.math.BigDecimal;
import java.time.LocalDateTime;

/**
 * 出荷検査結果データ
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ShipmentInspectionResult {
    private Integer id;
    private String shipmentInspectionNumber;
    private String defectCode;
    private BigDecimal quantity;
    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;
}
