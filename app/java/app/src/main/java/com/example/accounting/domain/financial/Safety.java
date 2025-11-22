package com.example.accounting.domain.financial;

import java.math.BigDecimal;

/**
 * 安全性指標
 */
public record Safety(
    BigDecimal currentRatio,  // 流動比率
    BigDecimal quickRatio,    // 当座比率
    BigDecimal equityRatio    // 自己資本比率
) {}
