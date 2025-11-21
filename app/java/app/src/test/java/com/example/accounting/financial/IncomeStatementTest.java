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
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * 損益計算書生成のテスト（Red フェーズ）
 */
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
@DisplayName("損益計算書 - 生成テスト")
class IncomeStatementTest extends TestDatabaseConfig {

    private FinancialStatementService financialStatementService;

    @BeforeAll
    static void setUp() throws Exception {
        // テスト用勘定科目を登録（損益計算書科目）
        insertTestPLAccounts();
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
    @DisplayName("損益計算書を生成できる")
    void testGenerateIncomeStatement() throws SQLException {
        // Given: 2024-01-01から2024-01-31の期間のデータ
        LocalDate fromDate = LocalDate.of(2024, 1, 1);
        LocalDate toDate = LocalDate.of(2024, 1, 31);
        insertTestPLBalanceData(fromDate, toDate);

        financialStatementService = new FinancialStatementService(
                postgres.getJdbcUrl(),
                postgres.getUsername(),
                postgres.getPassword()
        );

        // When: 損益計算書を生成
        IncomeStatement incomeStatement = financialStatementService.generateIncomeStatement(fromDate, toDate);

        // Then: 損益計算書のプロパティが存在する
        assertThat(incomeStatement).isNotNull();
        assertThat(incomeStatement.getFromDate()).isEqualTo(fromDate);
        assertThat(incomeStatement.getToDate()).isEqualTo(toDate);
        assertThat(incomeStatement.getRevenues()).isNotEmpty();
        assertThat(incomeStatement.getExpenses()).isNotEmpty();
        assertThat(incomeStatement.getTotalRevenues()).isGreaterThan(BigDecimal.ZERO);
        assertThat(incomeStatement.getTotalExpenses()).isGreaterThan(BigDecimal.ZERO);
    }

    @Test
    @Order(2)
    @DisplayName("収益・費用・利益項目が正しく計算されている")
    void testProfitCalculation() throws SQLException {
        // Given: 2024-01-01から2024-01-31の期間のデータ
        LocalDate fromDate = LocalDate.of(2024, 1, 1);
        LocalDate toDate = LocalDate.of(2024, 1, 31);
        insertTestPLBalanceData(fromDate, toDate);

        financialStatementService = new FinancialStatementService(
                postgres.getJdbcUrl(),
                postgres.getUsername(),
                postgres.getPassword()
        );

        // When: 損益計算書を生成
        IncomeStatement incomeStatement = financialStatementService.generateIncomeStatement(fromDate, toDate);

        // Then: 利益項目が計算されている
        assertThat(incomeStatement.getNetIncome()).isNotNull();
        assertThat(incomeStatement.getGrossProfit()).isNotNull();
        assertThat(incomeStatement.getOperatingIncome()).isNotNull();

        // 当期純利益 = 総収益 - 総費用
        BigDecimal expectedNetIncome = incomeStatement.getTotalRevenues()
                .subtract(incomeStatement.getTotalExpenses());
        assertThat(incomeStatement.getNetIncome())
                .isEqualByComparingTo(expectedNetIncome);

        // 具体的な金額の検証
        // 収益: 売上高10,000,000
        assertThat(incomeStatement.getTotalRevenues())
                .isEqualByComparingTo(new BigDecimal("10000000"));

        // 費用: 売上原価6,000,000 + 給料手当1,000,000 + 広告宣伝費500,000 = 7,500,000
        assertThat(incomeStatement.getTotalExpenses())
                .isEqualByComparingTo(new BigDecimal("7500000"));

        // 当期純利益: 10,000,000 - 7,500,000 = 2,500,000
        assertThat(incomeStatement.getNetIncome())
                .isEqualByComparingTo(new BigDecimal("2500000"));
    }

    @Test
    @Order(3)
    @DisplayName("収益項目が正しく分類されている")
    void testRevenueClassification() throws SQLException {
        // Given: 2024-01-01から2024-01-31の期間のデータ
        LocalDate fromDate = LocalDate.of(2024, 1, 1);
        LocalDate toDate = LocalDate.of(2024, 1, 31);
        insertTestPLBalanceData(fromDate, toDate);

        financialStatementService = new FinancialStatementService(
                postgres.getJdbcUrl(),
                postgres.getUsername(),
                postgres.getPassword()
        );

        // When: 損益計算書を生成
        IncomeStatement incomeStatement = financialStatementService.generateIncomeStatement(fromDate, toDate);

        // Then: 収益項目が存在する
        List<IncomeStatementItem> revenues = incomeStatement.getRevenues();
        assertThat(revenues).hasSize(1);

        // 売上高（41で始まる勘定科目）
        List<IncomeStatementItem> sales = revenues.stream()
                .filter(r -> r.getAccountCode().startsWith("41"))
                .toList();
        assertThat(sales).hasSize(1);
        assertThat(sales.get(0).getAccountCode()).isEqualTo("4110");
        assertThat(sales.get(0).getAccountName()).isEqualTo("売上高");
        assertThat(sales.get(0).getBalance())
                .isEqualByComparingTo(new BigDecimal("10000000"));
    }

    @Test
    @Order(4)
    @DisplayName("費用項目が正しく分類されている")
    void testExpenseClassification() throws SQLException {
        // Given: 2024-01-01から2024-01-31の期間のデータ
        LocalDate fromDate = LocalDate.of(2024, 1, 1);
        LocalDate toDate = LocalDate.of(2024, 1, 31);
        insertTestPLBalanceData(fromDate, toDate);

        financialStatementService = new FinancialStatementService(
                postgres.getJdbcUrl(),
                postgres.getUsername(),
                postgres.getPassword()
        );

        // When: 損益計算書を生成
        IncomeStatement incomeStatement = financialStatementService.generateIncomeStatement(fromDate, toDate);

        // Then: 費用項目が存在する
        List<IncomeStatementItem> expenses = incomeStatement.getExpenses();
        assertThat(expenses).hasSize(3);

        // 売上原価（51で始まる勘定科目）
        List<IncomeStatementItem> costOfSales = expenses.stream()
                .filter(e -> e.getAccountCode().startsWith("51"))
                .toList();
        assertThat(costOfSales).hasSize(1);
        assertThat(costOfSales.get(0).getAccountCode()).isEqualTo("5110");
        assertThat(costOfSales.get(0).getAccountName()).isEqualTo("売上原価");
        assertThat(costOfSales.get(0).getBalance())
                .isEqualByComparingTo(new BigDecimal("6000000"));

        // 販管費（6で始まる勘定科目）
        List<IncomeStatementItem> operatingExpenses = expenses.stream()
                .filter(e -> e.getAccountCode().startsWith("6"))
                .toList();
        assertThat(operatingExpenses).hasSize(2);
    }

    @Test
    @Order(5)
    @DisplayName("対売上比が計算されている")
    void testSalesPercentage() throws SQLException {
        // Given: 2024-01-01から2024-01-31の期間のデータ
        LocalDate fromDate = LocalDate.of(2024, 1, 1);
        LocalDate toDate = LocalDate.of(2024, 1, 31);
        insertTestPLBalanceData(fromDate, toDate);

        financialStatementService = new FinancialStatementService(
                postgres.getJdbcUrl(),
                postgres.getUsername(),
                postgres.getPassword()
        );

        // When: 損益計算書を生成
        IncomeStatement incomeStatement = financialStatementService.generateIncomeStatement(fromDate, toDate);

        // Then: 各項目の対売上比が存在し、0〜100の範囲内
        for (IncomeStatementItem revenue : incomeStatement.getRevenues()) {
            assertThat(revenue.getPercentage()).isNotNull();
            assertThat(revenue.getPercentage()).isBetween(BigDecimal.ZERO, new BigDecimal("100"));
        }

        for (IncomeStatementItem expense : incomeStatement.getExpenses()) {
            assertThat(expense.getPercentage()).isNotNull();
            assertThat(expense.getPercentage()).isBetween(BigDecimal.ZERO, new BigDecimal("100"));
        }

        // 売上高の対売上比：10,000,000 / 10,000,000 = 100%
        IncomeStatementItem sales = incomeStatement.getRevenues().stream()
                .filter(r -> r.getAccountCode().equals("4110"))
                .findFirst()
                .orElseThrow();
        assertThat(sales.getPercentage()).isEqualByComparingTo(new BigDecimal("100.00"));
    }

    /**
     * テスト用勘定科目を登録（損益計算書科目）
     */
    private static void insertTestPLAccounts() {
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
     * テスト用残高データを登録（損益計算書データ）
     */
    private void insertTestPLBalanceData(LocalDate fromDate, LocalDate toDate) throws SQLException {
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
