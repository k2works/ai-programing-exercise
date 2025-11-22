package com.example.management.domain;

import java.math.BigDecimal;

/**
 * 財務比率
 */
public class FinancialRatios {
    private BigDecimal operatingProfitMargin;  // 営業利益率
    private BigDecimal returnOnAssets;          // 総資産利益率 (ROA)
    private BigDecimal debtRatio;               // 負債比率

    public FinancialRatios(BigDecimal operatingProfitMargin, BigDecimal returnOnAssets, BigDecimal debtRatio) {
        this.operatingProfitMargin = operatingProfitMargin;
        this.returnOnAssets = returnOnAssets;
        this.debtRatio = debtRatio;
    }

    // Getters
    public BigDecimal getOperatingProfitMargin() {
        return operatingProfitMargin;
    }

    public BigDecimal getReturnOnAssets() {
        return returnOnAssets;
    }

    public BigDecimal getDebtRatio() {
        return debtRatio;
    }
}
