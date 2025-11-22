package com.example.accounting.domain.model.management.financial;

import java.math.BigDecimal;

/**
 * 効率性指標
 */
public record Efficiency(
    BigDecimal totalAssetTurnover,           // 総資本回転率
    BigDecimal tangibleFixedAssetTurnover    // 有形固定資産回転率
) {}
