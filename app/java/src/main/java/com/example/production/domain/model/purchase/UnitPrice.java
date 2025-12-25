package com.example.production.domain.model.purchase;

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
public class UnitPrice {
    private Integer id;
    private String itemCode;
    private String supplierCode;
    @Builder.Default
    private BigDecimal lotUnitQuantity = BigDecimal.ONE;
    private LocalDate effectiveFrom;
    private LocalDate effectiveTo;
    private BigDecimal unitPrice;
    private LocalDateTime createdAt;
    private String createdBy;
    private LocalDateTime updatedAt;
    private String updatedBy;
}
