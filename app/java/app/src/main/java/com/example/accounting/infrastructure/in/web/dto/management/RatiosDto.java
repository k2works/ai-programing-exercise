package com.example.accounting.infrastructure.in.web.dto.management;

/**
 * 財務指標 DTO
 */
public record RatiosDto(
    ProfitabilityDto profitability,
    EfficiencyDto efficiency,
    SafetyDto safety
) {}
