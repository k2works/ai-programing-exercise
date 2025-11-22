package com.example.accounting.application.port.in;

/**
 * 財務指標 DTO
 */
public record RatiosDto(
    ProfitabilityDto profitability,
    EfficiencyDto efficiency,
    SafetyDto safety
) {}
