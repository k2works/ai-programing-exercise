package com.example.production.infrastructure.web.dto;

import com.example.production.domain.model.bom.BomExplosion;
import lombok.Builder;
import lombok.Value;

import java.math.BigDecimal;
import java.time.LocalDate;

/**
 * BOM 展開レスポンス
 */
@Value
@Builder
public class BomExplosionResponse {
    String parentItemCode;
    String childItemCode;
    LocalDate effectiveFrom;
    LocalDate effectiveTo;
    BigDecimal baseQuantity;
    BigDecimal requiredQuantity;
    BigDecimal defectRate;
    Integer sequence;
    Integer level;
    BigDecimal totalQuantity;

    public static BomExplosionResponse from(BomExplosion explosion) {
        return BomExplosionResponse.builder()
                .parentItemCode(explosion.getParentItemCode())
                .childItemCode(explosion.getChildItemCode())
                .effectiveFrom(explosion.getEffectiveFrom())
                .effectiveTo(explosion.getEffectiveTo())
                .baseQuantity(explosion.getBaseQuantity())
                .requiredQuantity(explosion.getRequiredQuantity())
                .defectRate(explosion.getDefectRate())
                .sequence(explosion.getSequence())
                .level(explosion.getLevel())
                .totalQuantity(explosion.getTotalQuantity())
                .build();
    }
}
