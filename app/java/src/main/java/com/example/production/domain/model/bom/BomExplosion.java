package com.example.production.domain.model.bom;

import lombok.*;
import java.math.BigDecimal;
import java.time.LocalDate;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class BomExplosion {
    private String parentItemCode;
    private String childItemCode;
    private LocalDate effectiveFrom;
    private LocalDate effectiveTo;
    private BigDecimal baseQuantity;
    private BigDecimal requiredQuantity;
    private BigDecimal defectRate;
    private Integer sequence;
    private Integer level;
    private BigDecimal totalQuantity;
}
