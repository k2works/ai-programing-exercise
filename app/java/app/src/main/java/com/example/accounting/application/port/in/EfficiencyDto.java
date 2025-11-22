package com.example.accounting.application.port.in;

import java.math.BigDecimal;

/**
 * 効率性指標 DTO
 */
public record EfficiencyDto(
    BigDecimal totalAssetTurnover,
    BigDecimal tangibleFixedAssetTurnover
) {}
