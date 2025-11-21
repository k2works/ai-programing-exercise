package com.example.accounting.financial;

import com.example.accounting.TestDatabaseConfig;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import java.math.BigDecimal;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.time.LocalDate;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * 財務指標計算のテスト（Red フェーズ）
 */
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
@DisplayName("財務指標 - 計算テスト")
class FinancialRatiosTest extends TestDatabaseConfig {

    private FinancialStatementService financialStatementService;

    @BeforeAll
    static void setUp() throws Exception {
        // テスト用勘定科目を登録
        insertTestAccounts();
    }

    @AfterEach
    void cleanup() throws SQLException {
        try (Connection conn = DriverManager.getConnection(
                postgres.getJdbcUrl(),
                postgres.getUsername(),
                postgres.getPassword())) {
            conn.createStatement().execute("DELETE FROM \"日次勘定科目残高\"");
        }
    }

    @Test
    @Order(1)
    @DisplayName("財務指標を計算できる")
    void testCalculateFinancialRatios() throws SQLException {
        // Given: 貸借対照表と損益計算書のデータ
        LocalDate asOfDate = LocalDate.of(2024, 1, 31);
        LocalDate fromDate = LocalDate.of(2024, 1, 1);
        LocalDate toDate = LocalDate.of(2024, 1, 31);

        insertTestBalanceSheetData(asOfDate);
        insertTestIncomeStatementData(fromDate);

        financialStatementService = new FinancialStatementService(
                postgres.getJdbcUrl(),
                postgres.getUsername(),
                postgres.getPassword()
        );

        BalanceSheet balanceSheet = financialStatementService.generateBalanceSheet(asOfDate);
        IncomeStatement incomeStatement = financialStatementService.generateIncomeStatement(fromDate, toDate);

        // When: 財務指標を計算
        FinancialRatios ratios = financialStatementService.calculateFinancialRatios(
                balanceSheet, incomeStatement);

        // Then: 各指標が存在する
        assertThat(ratios).isNotNull();
        assertThat(ratios.getCurrentRatio()).isNotNull();
        assertThat(ratios.getDebtToEquityRatio()).isNotNull();
        assertThat(ratios.getGrossProfitMargin()).isNotNull();
        assertThat(ratios.getOperatingProfitMargin()).isNotNull();
        assertThat(ratios.getNetProfitMargin()).isNotNull();
        assertThat(ratios.getRoa()).isNotNull();
        assertThat(ratios.getRoe()).isNotNull();
    }

    @Test
    @Order(2)
    @DisplayName("流動比率が正しく計算されている")
    void testCurrentRatio() throws SQLException {
        // Given: 貸借対照表のデータ
        LocalDate asOfDate = LocalDate.of(2024, 1, 31);
        LocalDate fromDate = LocalDate.of(2024, 1, 1);
        LocalDate toDate = LocalDate.of(2024, 1, 31);

        insertTestBalanceSheetData(asOfDate);
        insertTestIncomeStatementData(fromDate);

        financialStatementService = new FinancialStatementService(
                postgres.getJdbcUrl(),
                postgres.getUsername(),
                postgres.getPassword()
        );

        BalanceSheet balanceSheet = financialStatementService.generateBalanceSheet(asOfDate);
        IncomeStatement incomeStatement = financialStatementService.generateIncomeStatement(fromDate, toDate);

        // When: 財務指標を計算
        FinancialRatios ratios = financialStatementService.calculateFinancialRatios(
                balanceSheet, incomeStatement);

        // Then: 流動比率 = 流動資産 / 流動負債 * 100
        // 流動資産: 普通預金8,000,000
        // 流動負債: 買掛金500,000
        // 流動比率: 8,000,000 / 500,000 * 100 = 1600%
        assertThat(ratios.getCurrentRatio())
                .isEqualByComparingTo(new BigDecimal("1600.00"));
    }

    @Test
    @Order(3)
    @DisplayName("自己資本比率が正しく計算されている")
    void testDebtToEquityRatio() throws SQLException {
        // Given: 貸借対照表のデータ
        LocalDate asOfDate = LocalDate.of(2024, 1, 31);
        LocalDate fromDate = LocalDate.of(2024, 1, 1);
        LocalDate toDate = LocalDate.of(2024, 1, 31);

        insertTestBalanceSheetData(asOfDate);
        insertTestIncomeStatementData(fromDate);

        financialStatementService = new FinancialStatementService(
                postgres.getJdbcUrl(),
                postgres.getUsername(),
                postgres.getPassword()
        );

        BalanceSheet balanceSheet = financialStatementService.generateBalanceSheet(asOfDate);
        IncomeStatement incomeStatement = financialStatementService.generateIncomeStatement(fromDate, toDate);

        // When: 財務指標を計算
        FinancialRatios ratios = financialStatementService.calculateFinancialRatios(
                balanceSheet, incomeStatement);

        // Then: 自己資本比率 = 純資産 / 総資産 * 100
        // 純資産: 5,000,000
        // 総資産: 10,000,000
        // 自己資本比率: 5,000,000 / 10,000,000 * 100 = 50%
        assertThat(ratios.getDebtToEquityRatio())
                .isEqualByComparingTo(new BigDecimal("50.00"));
    }

    @Test
    @Order(4)
    @DisplayName("収益性指標が正しく計算されている")
    void testProfitabilityRatios() throws SQLException {
        // Given: 損益計算書のデータ
        LocalDate asOfDate = LocalDate.of(2024, 1, 31);
        LocalDate fromDate = LocalDate.of(2024, 1, 1);
        LocalDate toDate = LocalDate.of(2024, 1, 31);

        insertTestBalanceSheetData(asOfDate);
        insertTestIncomeStatementData(fromDate);

        financialStatementService = new FinancialStatementService(
                postgres.getJdbcUrl(),
                postgres.getUsername(),
                postgres.getPassword()
        );

        BalanceSheet balanceSheet = financialStatementService.generateBalanceSheet(asOfDate);
        IncomeStatement incomeStatement = financialStatementService.generateIncomeStatement(fromDate, toDate);

        // When: 財務指標を計算
        FinancialRatios ratios = financialStatementService.calculateFinancialRatios(
                balanceSheet, incomeStatement);

        // Then: 売上総利益率 = 売上総利益 / 売上高 * 100
        // 売上総利益: 10,000,000 - 6,000,000 = 4,000,000
        // 売上高: 10,000,000
        // 売上総利益率: 4,000,000 / 10,000,000 * 100 = 40%
        assertThat(ratios.getGrossProfitMargin())
                .isEqualByComparingTo(new BigDecimal("40.00"));

        // 営業利益率 = 営業利益 / 売上高 * 100
        // 営業利益: 4,000,000 - 1,500,000 = 2,500,000
        // 営業利益率: 2,500,000 / 10,000,000 * 100 = 25%
        assertThat(ratios.getOperatingProfitMargin())
                .isEqualByComparingTo(new BigDecimal("25.00"));

        // 当期純利益率 = 当期純利益 / 売上高 * 100
        // 当期純利益: 10,000,000 - 7,500,000 = 2,500,000
        // 当期純利益率: 2,500,000 / 10,000,000 * 100 = 25%
        assertThat(ratios.getNetProfitMargin())
                .isEqualByComparingTo(new BigDecimal("25.00"));
    }

    @Test
    @Order(5)
    @DisplayName("ROAとROEが正しく計算されている")
    void testRoaAndRoe() throws SQLException {
        // Given: 貸借対照表と損益計算書のデータ
        LocalDate asOfDate = LocalDate.of(2024, 1, 31);
        LocalDate fromDate = LocalDate.of(2024, 1, 1);
        LocalDate toDate = LocalDate.of(2024, 1, 31);

        insertTestBalanceSheetData(asOfDate);
        insertTestIncomeStatementData(fromDate);

        financialStatementService = new FinancialStatementService(
                postgres.getJdbcUrl(),
                postgres.getUsername(),
                postgres.getPassword()
        );

        BalanceSheet balanceSheet = financialStatementService.generateBalanceSheet(asOfDate);
        IncomeStatement incomeStatement = financialStatementService.generateIncomeStatement(fromDate, toDate);

        // When: 財務指標を計算
        FinancialRatios ratios = financialStatementService.calculateFinancialRatios(
                balanceSheet, incomeStatement);

        // Then: ROA = 当期純利益 / 総資産 * 100
        // 当期純利益: 2,500,000
        // 総資産: 10,000,000
        // ROA: 2,500,000 / 10,000,000 * 100 = 25%
        assertThat(ratios.getRoa())
                .isEqualByComparingTo(new BigDecimal("25.00"));

        // ROE = 当期純利益 / 純資産 * 100
        // 当期純利益: 2,500,000
        // 純資産: 5,000,000
        // ROE: 2,500,000 / 5,000,000 * 100 = 50%
        assertThat(ratios.getRoe())
                .isEqualByComparingTo(new BigDecimal("50.00"));
    }

    /**
     * テスト用勘定科目を登録（貸借対照表と損益計算書）
     */
    private static void insertTestAccounts() {
        try (Connection conn = DriverManager.getConnection(
                postgres.getJdbcUrl(),
                postgres.getUsername(),
                postgres.getPassword())) {
            conn.createStatement().executeUpdate("""
                INSERT INTO "勘定科目マスタ" (
                    "勘定科目コード", "勘定科目名", "勘定科目種別", "BSPL区分", "取引要素区分",
                    "合計科目", "集計対象", "残高"
                )
                VALUES
                -- 貸借対照表科目
                ('1110', '普通預金', '資産'::account_type, 'B', '1', false, true, 0),
                ('1410', '建物', '資産'::account_type, 'B', '1', false, true, 0),
                ('2110', '買掛金', '負債'::account_type, 'B', '2', false, true, 0),
                ('2510', '長期借入金', '負債'::account_type, 'B', '2', false, true, 0),
                ('3110', '資本金', '純資産'::account_type, 'B', '3', false, true, 0),
                -- 損益計算書科目
                ('4110', '売上高', '収益'::account_type, 'P', '4', false, true, 0),
                ('5110', '売上原価', '費用'::account_type, 'P', '5', false, true, 0),
                ('6110', '給料手当', '費用'::account_type, 'P', '5', false, true, 0),
                ('6210', '広告宣伝費', '費用'::account_type, 'P', '5', false, true, 0)
                ON CONFLICT ("勘定科目コード") DO NOTHING
                """);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * テスト用残高データを登録（貸借対照表）
     */
    private void insertTestBalanceSheetData(LocalDate asOfDate) throws SQLException {
        try (Connection conn = DriverManager.getConnection(
                postgres.getJdbcUrl(),
                postgres.getUsername(),
                postgres.getPassword())) {
            var pstmt = conn.prepareStatement("""
                INSERT INTO "日次勘定科目残高" (
                    "起票日", "勘定科目コード", "補助科目コード", "部門コード",
                    "プロジェクトコード", "決算仕訳フラグ", "借方金額", "貸方金額"
                ) VALUES
                (?, '1110', '', '', '', 0, 8000000, 0),
                (?, '1410', '', '', '', 0, 2000000, 0),
                (?, '2110', '', '', '', 0, 0, 500000),
                (?, '2510', '', '', '', 0, 0, 4500000),
                (?, '3110', '', '', '', 0, 0, 5000000)
                """);
            pstmt.setObject(1, asOfDate);
            pstmt.setObject(2, asOfDate);
            pstmt.setObject(3, asOfDate);
            pstmt.setObject(4, asOfDate);
            pstmt.setObject(5, asOfDate);
            pstmt.executeUpdate();
        }
    }

    /**
     * テスト用残高データを登録（損益計算書）
     */
    private void insertTestIncomeStatementData(LocalDate fromDate) throws SQLException {
        try (Connection conn = DriverManager.getConnection(
                postgres.getJdbcUrl(),
                postgres.getUsername(),
                postgres.getPassword())) {
            var pstmt = conn.prepareStatement("""
                INSERT INTO "日次勘定科目残高" (
                    "起票日", "勘定科目コード", "補助科目コード", "部門コード",
                    "プロジェクトコード", "決算仕訳フラグ", "借方金額", "貸方金額"
                ) VALUES
                (?, '4110', '', '', '', 0, 0, 10000000),
                (?, '5110', '', '', '', 0, 6000000, 0),
                (?, '6110', '', '', '', 0, 1000000, 0),
                (?, '6210', '', '', '', 0, 500000, 0)
                """);
            pstmt.setObject(1, fromDate);
            pstmt.setObject(2, fromDate);
            pstmt.setObject(3, fromDate);
            pstmt.setObject(4, fromDate);
            pstmt.executeUpdate();
        }
    }
}
