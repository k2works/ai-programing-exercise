package com.example.accounting.infrastructure.in.web.dto.management;

import java.math.BigDecimal;

/**
 * 効率性指標 DTO
 */
public record EfficiencyDto(
    BigDecimal totalAssetTurnover,
    BigDecimal tangibleFixedAssetTurnover
) {}
