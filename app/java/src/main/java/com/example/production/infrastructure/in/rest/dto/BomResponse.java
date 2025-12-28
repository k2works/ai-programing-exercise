package com.example.production.infrastructure.in.rest.dto;

import com.example.production.domain.model.bom.Bom;
import lombok.Builder;
import lombok.Value;

import java.math.BigDecimal;
import java.time.LocalDate;

/**
 * BOM レスポンス
 */
@Value
@Builder
public class BomResponse {
    String parentItemCode;
    String childItemCode;
    LocalDate effectiveFrom;
    LocalDate effectiveTo;
    BigDecimal baseQuantity;
    BigDecimal requiredQuantity;
    BigDecimal defectRate;
    Integer sequence;

    public static BomResponse from(Bom bom) {
        return BomResponse.builder()
                .parentItemCode(bom.getParentItemCode())
                .childItemCode(bom.getChildItemCode())
                .effectiveFrom(bom.getEffectiveFrom())
                .effectiveTo(bom.getEffectiveTo())
                .baseQuantity(bom.getBaseQuantity())
                .requiredQuantity(bom.getRequiredQuantity())
                .defectRate(bom.getDefectRate())
                .sequence(bom.getSequence())
                .build();
    }
}
