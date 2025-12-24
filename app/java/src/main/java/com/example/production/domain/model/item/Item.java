package com.example.production.domain.model.item;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class Item {
    private Integer id;
    private String itemCode;
    private LocalDate effectiveFrom;
    private LocalDate effectiveTo;
    private String itemName;
    private ItemCategory itemCategory;
    private String unitCode;
    @Builder.Default
    private Integer leadTime = 0;
    @Builder.Default
    private Integer safetyLeadTime = 0;
    @Builder.Default
    private BigDecimal safetyStock = BigDecimal.ZERO;
    @Builder.Default
    private BigDecimal yieldRate = new BigDecimal("100");
    @Builder.Default
    private BigDecimal minLotSize = BigDecimal.ONE;
    @Builder.Default
    private BigDecimal lotIncrement = BigDecimal.ONE;
    private BigDecimal maxLotSize;
    private Integer shelfLife;
    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;
}
