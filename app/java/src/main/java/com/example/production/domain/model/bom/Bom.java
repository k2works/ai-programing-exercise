package com.example.production.domain.model.bom;

import lombok.*;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class Bom {
    private String parentItemCode;
    private String childItemCode;
    private LocalDate effectiveFrom;
    private LocalDate effectiveTo;
    @Builder.Default
    private BigDecimal baseQuantity = BigDecimal.ONE;
    private BigDecimal requiredQuantity;
    @Builder.Default
    private BigDecimal defectRate = BigDecimal.ZERO;
    private Integer sequence;
    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;
}
