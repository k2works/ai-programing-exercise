package com.example.accounting.application.model.financial;

import java.math.BigDecimal;

/**
 * 財務指標
 */
public class FinancialRatios {
    private BigDecimal currentRatio;           // 流動比率（％）
    private BigDecimal debtToEquityRatio;      // 自己資本比率（％）
    private BigDecimal grossProfitMargin;      // 売上総利益率（％）
    private BigDecimal operatingProfitMargin;  // 営業利益率（％）
    private BigDecimal netProfitMargin;        // 当期純利益率（％）
    private BigDecimal roa;                    // 総資産利益率（ROA, ％）
    private BigDecimal roe;                    // 自己資本利益率（ROE, ％）

    public FinancialRatios() {
    }

    @SuppressWarnings("checkstyle:ParameterNumber")
    public FinancialRatios(BigDecimal currentRatio, BigDecimal debtToEquityRatio,
                           BigDecimal grossProfitMargin, BigDecimal operatingProfitMargin,
                           BigDecimal netProfitMargin, BigDecimal roa, BigDecimal roe) {
        this.currentRatio = currentRatio;
        this.debtToEquityRatio = debtToEquityRatio;
        this.grossProfitMargin = grossProfitMargin;
        this.operatingProfitMargin = operatingProfitMargin;
        this.netProfitMargin = netProfitMargin;
        this.roa = roa;
        this.roe = roe;
    }

    public BigDecimal getCurrentRatio() {
        return currentRatio;
    }

    public void setCurrentRatio(BigDecimal currentRatio) {
        this.currentRatio = currentRatio;
    }

    public BigDecimal getDebtToEquityRatio() {
        return debtToEquityRatio;
    }

    public void setDebtToEquityRatio(BigDecimal debtToEquityRatio) {
        this.debtToEquityRatio = debtToEquityRatio;
    }

    public BigDecimal getGrossProfitMargin() {
        return grossProfitMargin;
    }

    public void setGrossProfitMargin(BigDecimal grossProfitMargin) {
        this.grossProfitMargin = grossProfitMargin;
    }

    public BigDecimal getOperatingProfitMargin() {
        return operatingProfitMargin;
    }

    public void setOperatingProfitMargin(BigDecimal operatingProfitMargin) {
        this.operatingProfitMargin = operatingProfitMargin;
    }

    public BigDecimal getNetProfitMargin() {
        return netProfitMargin;
    }

    public void setNetProfitMargin(BigDecimal netProfitMargin) {
        this.netProfitMargin = netProfitMargin;
    }

    public BigDecimal getRoa() {
        return roa;
    }

    public void setRoa(BigDecimal roa) {
        this.roa = roa;
    }

    public BigDecimal getRoe() {
        return roe;
    }

    public void setRoe(BigDecimal roe) {
        this.roe = roe;
    }
}
