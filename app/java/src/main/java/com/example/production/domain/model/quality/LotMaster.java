package com.example.production.domain.model.quality;

import lombok.*;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;

/**
 * ロットマスタ
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class LotMaster {
    private Integer id;
    private String lotNumber;
    private String itemCode;
    private LotType lotType;
    private LocalDate productionDate;
    private LocalDate expirationDate;
    private BigDecimal quantity;
    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;
}
