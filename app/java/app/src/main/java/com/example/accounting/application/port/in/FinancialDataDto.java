package com.example.accounting.application.port.in;

import java.math.BigDecimal;

/**
 * 財務データ DTO
 */
public record FinancialDataDto(
    BigDecimal sales,
    BigDecimal operatingProfit,
    BigDecimal totalAssets,
    BigDecimal tangibleFixedAssets,
    BigDecimal currentAssets,
    BigDecimal currentLiabilities,
    BigDecimal quickAssets,
    BigDecimal equity
) {}
