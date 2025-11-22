package com.example.accounting.application.port.in;

import java.math.BigDecimal;

/**
 * 安全性指標 DTO
 */
public record SafetyDto(
    BigDecimal currentRatio,
    BigDecimal quickRatio,
    BigDecimal equityRatio
) {}
