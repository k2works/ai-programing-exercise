package com.example.accounting.infrastructure.in.web.dto;

import com.example.accounting.domain.model.financial.FinancialRatios;

import java.math.BigDecimal;

/**
 * 財務指標レスポンスDTO
 */
public record FinancialRatiosResponse(
        BigDecimal currentRatio,
        BigDecimal debtToEquityRatio,
        BigDecimal grossProfitMargin,
        BigDecimal operatingProfitMargin,
        BigDecimal netProfitMargin,
        BigDecimal roa,
        BigDecimal roe
) {
    /**
     * ドメインモデルからDTOに変換
     */
    public static FinancialRatiosResponse from(FinancialRatios ratios) {
        return new FinancialRatiosResponse(
                ratios.getCurrentRatio(),
                ratios.getDebtToEquityRatio(),
                ratios.getGrossProfitMargin(),
                ratios.getOperatingProfitMargin(),
                ratios.getNetProfitMargin(),
                ratios.getRoa(),
                ratios.getRoe()
        );
    }
}
