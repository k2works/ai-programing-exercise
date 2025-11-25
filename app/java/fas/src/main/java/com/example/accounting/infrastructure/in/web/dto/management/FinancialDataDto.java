package com.example.accounting.infrastructure.in.web.dto.management;

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
