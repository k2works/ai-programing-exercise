package com.example.accounting.domain.financial;

import java.math.BigDecimal;

/**
 * 収益性指標
 */
public record Profitability(
    BigDecimal operatingProfitMargin  // 売上高営業利益率
) {}
