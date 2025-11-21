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
}
