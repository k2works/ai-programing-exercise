package com.example.accounting.financial;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;

/**
 * 財務諸表生成サービス
 */
public class FinancialStatementService {

    private final String jdbcUrl;
    private final String username;
    private final String password;

    public FinancialStatementService(String jdbcUrl, String username, String password) {
        this.jdbcUrl = jdbcUrl;
        this.username = username;
        this.password = password;
    }

    /**
     * 貸借対照表を生成する
     *
     * @param asOfDate 基準日
     * @return 貸借対照表
     */
    public BalanceSheet generateBalanceSheet(LocalDate asOfDate) {
        try (Connection conn = DriverManager.getConnection(jdbcUrl, username, password)) {

            // 資産を取得（取引要素区分='1'）
            List<BalanceSheetItem> assets = fetchBalanceSheetItems(conn, asOfDate, "1");

            // 負債を取得（取引要素区分='2'）
            List<BalanceSheetItem> liabilities = fetchBalanceSheetItems(conn, asOfDate, "2");

            // 純資産を取得（取引要素区分='3'）
            List<BalanceSheetItem> equity = fetchBalanceSheetItems(conn, asOfDate, "3");

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

        } catch (SQLException e) {
            throw new RuntimeException("Failed to generate balance sheet", e);
        }
    }

    /**
     * 貸借対照表の明細項目を取得
     */
    private List<BalanceSheetItem> fetchBalanceSheetItems(
            Connection conn, LocalDate asOfDate, String elementType) throws SQLException {

        String sql = """
            SELECT
                a."勘定科目コード" as account_code,
                a."勘定科目名" as account_name,
                CASE
                    WHEN a."勘定科目種別" IN ('資産', '費用') THEN
                        COALESCE(SUM(d."借方金額"), 0) - COALESCE(SUM(d."貸方金額"), 0)
                    ELSE
                        COALESCE(SUM(d."貸方金額"), 0) - COALESCE(SUM(d."借方金額"), 0)
                END as balance
            FROM "勘定科目マスタ" a
            LEFT JOIN "日次勘定科目残高" d
                ON a."勘定科目コード" = d."勘定科目コード"
                AND d."起票日" = ?
                AND d."決算仕訳フラグ" = 0
            WHERE a."BSPL区分" = 'B'
              AND a."取引要素区分" = ?
            GROUP BY a."勘定科目コード", a."勘定科目名", a."勘定科目種別"
            HAVING CASE
                WHEN a."勘定科目種別" IN ('資産', '費用') THEN
                    COALESCE(SUM(d."借方金額"), 0) - COALESCE(SUM(d."貸方金額"), 0)
                ELSE
                    COALESCE(SUM(d."貸方金額"), 0) - COALESCE(SUM(d."借方金額"), 0)
            END != 0
            ORDER BY a."勘定科目コード"
            """;

        List<BalanceSheetItem> items = new ArrayList<>();

        try (PreparedStatement pstmt = conn.prepareStatement(sql)) {
            pstmt.setObject(1, asOfDate);
            pstmt.setString(2, elementType);

            try (ResultSet rs = pstmt.executeQuery()) {
                while (rs.next()) {
                    String accountCode = rs.getString("account_code");
                    String accountName = rs.getString("account_name");
                    BigDecimal balance = rs.getBigDecimal("balance");

                    items.add(new BalanceSheetItem(accountCode, accountName, balance, BigDecimal.ZERO));
                }
            }
        }

        return items;
    }

    /**
     * 合計を計算
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
        try (Connection conn = DriverManager.getConnection(jdbcUrl, username, password)) {

            // 収益を取得（取引要素区分='4'）
            List<IncomeStatementItem> revenues = fetchIncomeStatementItems(conn, fromDate, toDate, "4");

            // 費用を取得（取引要素区分='5'）
            List<IncomeStatementItem> expenses = fetchIncomeStatementItems(conn, fromDate, toDate, "5");

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

        } catch (SQLException e) {
            throw new RuntimeException("Failed to generate income statement", e);
        }
    }

    /**
     * 損益計算書の明細項目を取得
     */
    private List<IncomeStatementItem> fetchIncomeStatementItems(
            Connection conn, LocalDate fromDate, LocalDate toDate, String elementType) throws SQLException {

        String sql = """
            SELECT
                a."勘定科目コード" as account_code,
                a."勘定科目名" as account_name,
                CASE
                    WHEN a."勘定科目種別" = '収益' THEN
                        COALESCE(SUM(d."貸方金額"), 0) - COALESCE(SUM(d."借方金額"), 0)
                    ELSE
                        COALESCE(SUM(d."借方金額"), 0) - COALESCE(SUM(d."貸方金額"), 0)
                END as balance
            FROM "勘定科目マスタ" a
            LEFT JOIN "日次勘定科目残高" d
                ON a."勘定科目コード" = d."勘定科目コード"
                AND d."起票日" BETWEEN ? AND ?
                AND d."決算仕訳フラグ" = 0
            WHERE a."BSPL区分" = 'P'
              AND a."取引要素区分" = ?
            GROUP BY a."勘定科目コード", a."勘定科目名", a."勘定科目種別"
            HAVING CASE
                WHEN a."勘定科目種別" = '収益' THEN
                    COALESCE(SUM(d."貸方金額"), 0) - COALESCE(SUM(d."借方金額"), 0)
                ELSE
                    COALESCE(SUM(d."借方金額"), 0) - COALESCE(SUM(d."貸方金額"), 0)
            END != 0
            ORDER BY a."勘定科目コード"
            """;

        List<IncomeStatementItem> items = new ArrayList<>();

        try (PreparedStatement pstmt = conn.prepareStatement(sql)) {
            pstmt.setObject(1, fromDate);
            pstmt.setObject(2, toDate);
            pstmt.setString(3, elementType);

            try (ResultSet rs = pstmt.executeQuery()) {
                while (rs.next()) {
                    String accountCode = rs.getString("account_code");
                    String accountName = rs.getString("account_name");
                    BigDecimal balance = rs.getBigDecimal("balance");

                    items.add(new IncomeStatementItem(accountCode, accountName, balance, BigDecimal.ZERO));
                }
            }
        }

        return items;
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
