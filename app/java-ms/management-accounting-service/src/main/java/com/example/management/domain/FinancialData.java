package com.example.management.domain;

import java.math.BigDecimal;
import java.util.List;

/**
 * 財務データ（管理会計ドメインモデル）
 */
public class FinancialData {
    private Integer fiscalYear;
    private BigDecimal sales;
    private BigDecimal operatingProfit;
    private BigDecimal totalAssets;
    private BigDecimal totalLiabilities;
    private List<Journal> journals;

    public FinancialData(Integer fiscalYear, List<Journal> journals) {
        this.fiscalYear = fiscalYear;
        this.journals = journals;
        // 簡易的な集計（本来はもっと複雑なビジネスロジック）
        this.sales = BigDecimal.ZERO;
        this.operatingProfit = BigDecimal.ZERO;
        this.totalAssets = BigDecimal.ZERO;
        this.totalLiabilities = BigDecimal.ZERO;
    }

    // Getters
    public Integer getFiscalYear() {
        return fiscalYear;
    }

    public BigDecimal getSales() {
        return sales;
    }

    public BigDecimal getOperatingProfit() {
        return operatingProfit;
    }

    public BigDecimal getTotalAssets() {
        return totalAssets;
    }

    public BigDecimal getTotalLiabilities() {
        return totalLiabilities;
    }

    public List<Journal> getJournals() {
        return journals;
    }
}
