package com.example.accounting.application.port.in;

import java.math.BigDecimal;

/**
 * トレンド DTO
 */
public record TrendsDto(
    BigDecimal operatingProfitMarginChange,
    BigDecimal totalAssetTurnoverChange,
    BigDecimal equityRatioChange
) {}
