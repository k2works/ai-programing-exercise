package com.example.accounting.application.port.in;

import java.math.BigDecimal;

/**
 * 収益性指標 DTO
 */
public record ProfitabilityDto(
    BigDecimal operatingProfitMargin
) {}
