package com.example.accounting.infrastructure.in.web.dto.management;

import java.math.BigDecimal;

/**
 * 収益性指標 DTO
 */
public record ProfitabilityDto(
    BigDecimal operatingProfitMargin
) {}
