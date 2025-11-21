package com.example.accounting.application.service;

import com.example.accounting.application.port.out.FinancialDataRepository;
import com.example.accounting.application.model.financial.BalanceSheet;
import com.example.accounting.application.model.financial.BalanceSheetItem;
import com.example.accounting.application.model.financial.FinancialRatios;
import com.example.accounting.application.model.financial.IncomeStatement;
import com.example.accounting.application.model.financial.IncomeStatementItem;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDate;
import java.util.List;

/**
 * 財務諸表生成サービス（Application Service）
 */
@Service
@Transactional(readOnly = true)
public class FinancialStatementService {

    private final FinancialDataRepository financialDataRepository;

    public FinancialStatementService(FinancialDataRepository financialDataRepository) {
        this.financialDataRepository = financialDataRepository;
    }

    /**
     * 貸借対照表を生成する
     *
     * @param asOfDate 基準日
     * @return 貸借対照表
     */
    public BalanceSheet generateBalanceSheet(LocalDate asOfDate) {
        // 資産を取得（取引要素区分='1'）
        List<BalanceSheetItem> assets = financialDataRepository.findBalanceSheetItems(asOfDate, "1");

        // 負債を取得（取引要素区分='2'）
        List<BalanceSheetItem> liabilities = financialDataRepository.findBalanceSheetItems(asOfDate, "2");

        // 純資産を取得（取引要素区分='3'）
        List<BalanceSheetItem> equity = financialDataRepository.findBalanceSheetItems(asOfDate, "3");

        // 合計を計算
        BigDecimal totalAssets = calculateTotal(assets);
        BigDecimal totalLiabilities = calculateTotal(liabilities);
        BigDecimal totalEquity = calculateTotal(equity);
        BigDecimal totalLiabilitiesAndEquity = totalLiabilities.add(totalEquity);

        // 構成比率を計算
        calculateRatios(assets, totalAssets);
        calculateRatios(liabilities, totalLiabilitiesAndEquity);
        calculateRatios(equity, totalLiabilitiesAndEquity);

        return new BalanceSheet(
                asOfDate,
                assets,
                liabilities,
                equity,
                totalAssets,
                totalLiabilities,
                totalEquity,
                totalLiabilitiesAndEquity
        );
    }

    /**
     * 貸借対照表の合計を計算
     */
    private BigDecimal calculateTotal(List<BalanceSheetItem> items) {
        return items.stream()
                .map(BalanceSheetItem::getBalance)
                .reduce(BigDecimal.ZERO, BigDecimal::add);
    }

    /**
     * 構成比率を計算（%）
     */
    private void calculateRatios(List<BalanceSheetItem> items, BigDecimal total) {
        if (total.compareTo(BigDecimal.ZERO) == 0) {
            return;
        }

        for (BalanceSheetItem item : items) {
            BigDecimal ratio = item.getBalance()
                    .multiply(new BigDecimal("100"))
                    .divide(total, 2, RoundingMode.HALF_UP);
            item.setRatio(ratio);
        }
    }

    /**
     * 損益計算書を生成する
     *
     * @param fromDate 開始日
     * @param toDate 終了日
     * @return 損益計算書
     */
    public IncomeStatement generateIncomeStatement(LocalDate fromDate, LocalDate toDate) {
        // 収益を取得（取引要素区分='4'）
        List<IncomeStatementItem> revenues = financialDataRepository.findIncomeStatementItems(
                fromDate, toDate, "4");

        // 費用を取得（取引要素区分='5'）
        List<IncomeStatementItem> expenses = financialDataRepository.findIncomeStatementItems(
                fromDate, toDate, "5");

        // 合計を計算
        BigDecimal totalRevenues = calculateIncomeStatementTotal(revenues);
        BigDecimal totalExpenses = calculateIncomeStatementTotal(expenses);

        // 売上原価（勘定科目コードが51で始まる）
        BigDecimal costOfSales = expenses.stream()
                .filter(e -> e.getAccountCode().startsWith("51"))
                .map(IncomeStatementItem::getBalance)
                .reduce(BigDecimal.ZERO, BigDecimal::add);

        // 販管費（勘定科目コードが6または52で始まる）
        BigDecimal operatingExpenses = expenses.stream()
                .filter(e -> e.getAccountCode().startsWith("6") || e.getAccountCode().startsWith("52"))
                .map(IncomeStatementItem::getBalance)
                .reduce(BigDecimal.ZERO, BigDecimal::add);

        // 利益項目の計算
        BigDecimal grossProfit = totalRevenues.subtract(costOfSales);
        BigDecimal operatingIncome = grossProfit.subtract(operatingExpenses);
        BigDecimal netIncome = totalRevenues.subtract(totalExpenses);

        // 対売上比を計算
        calculatePercentages(revenues, totalRevenues);
        calculatePercentages(expenses, totalRevenues);

        return new IncomeStatement(
                fromDate,
                toDate,
                revenues,
                expenses,
                grossProfit,
                operatingIncome,
                netIncome,
                totalRevenues,
                totalExpenses
        );
    }

    /**
     * 損益計算書の合計を計算
     */
    private BigDecimal calculateIncomeStatementTotal(List<IncomeStatementItem> items) {
        return items.stream()
                .map(IncomeStatementItem::getBalance)
                .reduce(BigDecimal.ZERO, BigDecimal::add);
    }

    /**
     * 対売上比を計算（%）
     */
    private void calculatePercentages(List<IncomeStatementItem> items, BigDecimal totalRevenues) {
        if (totalRevenues.compareTo(BigDecimal.ZERO) == 0) {
            return;
        }

        for (IncomeStatementItem item : items) {
            BigDecimal percentage = item.getBalance()
                    .multiply(new BigDecimal("100"))
                    .divide(totalRevenues, 2, RoundingMode.HALF_UP);
            item.setPercentage(percentage);
        }
    }

    /**
     * 財務指標を計算する
     *
     * @param balanceSheet 貸借対照表
     * @param incomeStatement 損益計算書
     * @return 財務指標
     */
    public FinancialRatios calculateFinancialRatios(
            BalanceSheet balanceSheet,
            IncomeStatement incomeStatement) {

        // 流動資産・流動負債の抽出
        BigDecimal currentAssets = balanceSheet.getAssets().stream()
                .filter(asset -> asset.getAccountCode().startsWith("11")
                        || asset.getAccountCode().startsWith("12"))
                .map(BalanceSheetItem::getBalance)
                .reduce(BigDecimal.ZERO, BigDecimal::add);

        BigDecimal currentLiabilities = balanceSheet.getLiabilities().stream()
                .filter(liability -> liability.getAccountCode().startsWith("21"))
                .map(BalanceSheetItem::getBalance)
                .reduce(BigDecimal.ZERO, BigDecimal::add);

        // 各種指標の計算
        BigDecimal currentRatio = currentLiabilities.compareTo(BigDecimal.ZERO) > 0
                ? currentAssets.divide(currentLiabilities, 10, RoundingMode.HALF_UP)
                        .multiply(new BigDecimal("100"))
                        .setScale(2, RoundingMode.HALF_UP)
                : BigDecimal.ZERO;

        BigDecimal debtToEquityRatio = balanceSheet.getTotalAssets().compareTo(BigDecimal.ZERO) > 0
                ? balanceSheet.getTotalEquity()
                        .divide(balanceSheet.getTotalAssets(), 10, RoundingMode.HALF_UP)
                        .multiply(new BigDecimal("100"))
                        .setScale(2, RoundingMode.HALF_UP)
                : BigDecimal.ZERO;

        BigDecimal grossProfitMargin = incomeStatement.getTotalRevenues().compareTo(BigDecimal.ZERO) > 0
                ? incomeStatement.getGrossProfit()
                        .divide(incomeStatement.getTotalRevenues(), 10, RoundingMode.HALF_UP)
                        .multiply(new BigDecimal("100"))
                        .setScale(2, RoundingMode.HALF_UP)
                : BigDecimal.ZERO;

        BigDecimal operatingProfitMargin = incomeStatement.getTotalRevenues().compareTo(BigDecimal.ZERO) > 0
                ? incomeStatement.getOperatingIncome()
                        .divide(incomeStatement.getTotalRevenues(), 10, RoundingMode.HALF_UP)
                        .multiply(new BigDecimal("100"))
                        .setScale(2, RoundingMode.HALF_UP)
                : BigDecimal.ZERO;

        BigDecimal netProfitMargin = incomeStatement.getTotalRevenues().compareTo(BigDecimal.ZERO) > 0
                ? incomeStatement.getNetIncome()
                        .divide(incomeStatement.getTotalRevenues(), 10, RoundingMode.HALF_UP)
                        .multiply(new BigDecimal("100"))
                        .setScale(2, RoundingMode.HALF_UP)
                : BigDecimal.ZERO;

        BigDecimal roa = balanceSheet.getTotalAssets().compareTo(BigDecimal.ZERO) > 0
                ? incomeStatement.getNetIncome()
                        .divide(balanceSheet.getTotalAssets(), 10, RoundingMode.HALF_UP)
                        .multiply(new BigDecimal("100"))
                        .setScale(2, RoundingMode.HALF_UP)
                : BigDecimal.ZERO;

        BigDecimal roe = balanceSheet.getTotalEquity().compareTo(BigDecimal.ZERO) > 0
                ? incomeStatement.getNetIncome()
                        .divide(balanceSheet.getTotalEquity(), 10, RoundingMode.HALF_UP)
                        .multiply(new BigDecimal("100"))
                        .setScale(2, RoundingMode.HALF_UP)
                : BigDecimal.ZERO;

        return new FinancialRatios(
                currentRatio,
                debtToEquityRatio,
                grossProfitMargin,
                operatingProfitMargin,
                netProfitMargin,
                roa,
                roe
        );
    }
}
