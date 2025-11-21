package com.example.accounting.application.model.financial;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;

/**
 * 損益計算書（Income Statement / P/L）
 */
public class IncomeStatement {
    private LocalDate fromDate;
    private LocalDate toDate;
    private List<IncomeStatementItem> revenues;    // 収益
    private List<IncomeStatementItem> expenses;    // 費用
    private BigDecimal grossProfit;                // 売上総利益
    private BigDecimal operatingIncome;            // 営業利益
    private BigDecimal netIncome;                  // 当期純利益
    private BigDecimal totalRevenues;              // 総収益
    private BigDecimal totalExpenses;              // 総費用

    public IncomeStatement() {
    }

    @SuppressWarnings("checkstyle:ParameterNumber")
    public IncomeStatement(LocalDate fromDate, LocalDate toDate,
                           List<IncomeStatementItem> revenues, List<IncomeStatementItem> expenses,
                           BigDecimal grossProfit, BigDecimal operatingIncome, BigDecimal netIncome,
                           BigDecimal totalRevenues, BigDecimal totalExpenses) {
        this.fromDate = fromDate;
        this.toDate = toDate;
        this.revenues = revenues;
        this.expenses = expenses;
        this.grossProfit = grossProfit;
        this.operatingIncome = operatingIncome;
        this.netIncome = netIncome;
        this.totalRevenues = totalRevenues;
        this.totalExpenses = totalExpenses;
    }

    public LocalDate getFromDate() {
        return fromDate;
    }

    public void setFromDate(LocalDate fromDate) {
        this.fromDate = fromDate;
    }

    public LocalDate getToDate() {
        return toDate;
    }

    public void setToDate(LocalDate toDate) {
        this.toDate = toDate;
    }

    public List<IncomeStatementItem> getRevenues() {
        return revenues;
    }

    public void setRevenues(List<IncomeStatementItem> revenues) {
        this.revenues = revenues;
    }

    public List<IncomeStatementItem> getExpenses() {
        return expenses;
    }

    public void setExpenses(List<IncomeStatementItem> expenses) {
        this.expenses = expenses;
    }

    public BigDecimal getGrossProfit() {
        return grossProfit;
    }

    public void setGrossProfit(BigDecimal grossProfit) {
        this.grossProfit = grossProfit;
    }

    public BigDecimal getOperatingIncome() {
        return operatingIncome;
    }

    public void setOperatingIncome(BigDecimal operatingIncome) {
        this.operatingIncome = operatingIncome;
    }

    public BigDecimal getNetIncome() {
        return netIncome;
    }

    public void setNetIncome(BigDecimal netIncome) {
        this.netIncome = netIncome;
    }

    public BigDecimal getTotalRevenues() {
        return totalRevenues;
    }

    public void setTotalRevenues(BigDecimal totalRevenues) {
        this.totalRevenues = totalRevenues;
    }

    public BigDecimal getTotalExpenses() {
        return totalExpenses;
    }

    public void setTotalExpenses(BigDecimal totalExpenses) {
        this.totalExpenses = totalExpenses;
    }
}
