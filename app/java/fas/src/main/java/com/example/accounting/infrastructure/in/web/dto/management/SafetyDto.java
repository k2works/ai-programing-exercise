package com.example.accounting.infrastructure.in.web.dto.management;

import java.math.BigDecimal;

/**
 * 安全性指標 DTO
 */
public record SafetyDto(
    BigDecimal currentRatio,
    BigDecimal quickRatio,
    BigDecimal equityRatio
) {}
