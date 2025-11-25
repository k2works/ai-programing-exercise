package com.example.accounting.infrastructure.in.web.dto.management;

import java.math.BigDecimal;

/**
 * トレンド DTO
 */
public record TrendsDto(
    BigDecimal operatingProfitMarginChange,
    BigDecimal totalAssetTurnoverChange,
    BigDecimal equityRatioChange
) {}
