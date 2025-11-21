package com.example.accounting.financial;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;

/**
 * 貸借対照表
 */
public class BalanceSheet {
    private LocalDate asOfDate;  // 基準日
    private List<BalanceSheetItem> assets;  // 資産
    private List<BalanceSheetItem> liabilities;  // 負債
    private List<BalanceSheetItem> equity;  // 純資産
    private BigDecimal totalAssets;  // 資産合計
    private BigDecimal totalLiabilities;  // 負債合計
    private BigDecimal totalEquity;  // 純資産合計
    private BigDecimal totalLiabilitiesAndEquity;  // 負債・純資産合計

    @SuppressWarnings("checkstyle:ParameterNumber")
    public BalanceSheet(LocalDate asOfDate, List<BalanceSheetItem> assets,
                        List<BalanceSheetItem> liabilities, List<BalanceSheetItem> equity,
                        BigDecimal totalAssets, BigDecimal totalLiabilities,
                        BigDecimal totalEquity, BigDecimal totalLiabilitiesAndEquity) {
        this.asOfDate = asOfDate;
        this.assets = assets;
        this.liabilities = liabilities;
        this.equity = equity;
        this.totalAssets = totalAssets;
        this.totalLiabilities = totalLiabilities;
        this.totalEquity = totalEquity;
        this.totalLiabilitiesAndEquity = totalLiabilitiesAndEquity;
    }

    public LocalDate getAsOfDate() {
        return asOfDate;
    }

    public void setAsOfDate(LocalDate asOfDate) {
        this.asOfDate = asOfDate;
    }

    public List<BalanceSheetItem> getAssets() {
        return assets;
    }

    public void setAssets(List<BalanceSheetItem> assets) {
        this.assets = assets;
    }

    public List<BalanceSheetItem> getLiabilities() {
        return liabilities;
    }

    public void setLiabilities(List<BalanceSheetItem> liabilities) {
        this.liabilities = liabilities;
    }

    public List<BalanceSheetItem> getEquity() {
        return equity;
    }

    public void setEquity(List<BalanceSheetItem> equity) {
        this.equity = equity;
    }

    public BigDecimal getTotalAssets() {
        return totalAssets;
    }

    public void setTotalAssets(BigDecimal totalAssets) {
        this.totalAssets = totalAssets;
    }

    public BigDecimal getTotalLiabilities() {
        return totalLiabilities;
    }

    public void setTotalLiabilities(BigDecimal totalLiabilities) {
        this.totalLiabilities = totalLiabilities;
    }

    public BigDecimal getTotalEquity() {
        return totalEquity;
    }

    public void setTotalEquity(BigDecimal totalEquity) {
        this.totalEquity = totalEquity;
    }

    public BigDecimal getTotalLiabilitiesAndEquity() {
        return totalLiabilitiesAndEquity;
    }

    public void setTotalLiabilitiesAndEquity(BigDecimal totalLiabilitiesAndEquity) {
        this.totalLiabilitiesAndEquity = totalLiabilitiesAndEquity;
    }
}
