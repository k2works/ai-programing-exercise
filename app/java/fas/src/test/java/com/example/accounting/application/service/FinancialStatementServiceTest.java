package com.example.accounting.application.service;

import com.example.accounting.TestDatabaseConfig;
import com.example.accounting.application.port.out.FinancialDataRepository;
import com.example.accounting.application.service.finacial.FinancialStatementService;
import com.example.accounting.domain.model.financial.BalanceSheet;
import com.example.accounting.domain.model.financial.FinancialRatios;
import com.example.accounting.domain.model.financial.IncomeStatement;
import com.example.accounting.infrastructure.out.persistence.adapter.FinancialDataAdapter;
import com.example.accounting.infrastructure.out.persistence.mapper.FinancialDataMapper;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.time.LocalDate;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * 財務諸表サービスの統合テスト
 */
@DisplayName("財務諸表サービス - 統合テスト")
class FinancialStatementServiceTest extends TestDatabaseConfig {

    private FinancialStatementService financialStatementService;
    private FinancialDataMapper mapper;

    @BeforeAll
    static void setUpOnce() throws Exception {
        // テスト用勘定科目を登録
        insertTestAccounts();
    }

    /**
     * テスト用勘定科目を登録
     */
    private static void insertTestAccounts() {
        try (Connection conn = DriverManager.getConnection(
                POSTGRES.getJdbcUrl(),
                POSTGRES.getUsername(),
                POSTGRES.getPassword())) {
            conn.createStatement().executeUpdate("""
                INSERT INTO "勘定科目マスタ" ("勘定科目コード", "勘定科目名", "勘定科目種別", "BSPL区分", "取引要素区分", "合計科目", "集計対象", "残高")
                VALUES
                -- 資産（取引要素区分=1）
                ('1110', '現金', '資産'::account_type, 'B', '1', false, true, 0),
                ('1120', '普通預金', '資産'::account_type, 'B', '1', false, true, 0),
                ('1210', '売掛金', '資産'::account_type, 'B', '1', false, true, 0),
                -- 負債（取引要素区分=2）
                ('2110', '買掛金', '負債'::account_type, 'B', '2', false, true, 0),
                ('2120', '短期借入金', '負債'::account_type, 'B', '2', false, true, 0),
                -- 純資産（取引要素区分=3）
                ('3110', '資本金', '純資産'::account_type, 'B', '3', false, true, 0),
                -- 収益（取引要素区分=4）
                ('4110', '売上高', '収益'::account_type, 'P', '4', false, true, 0),
                -- 費用（取引要素区分=5）
                ('5110', '売上原価', '費用'::account_type, 'P', '5', false, true, 0),
                ('6110', '販売費', '費用'::account_type, 'P', '5', false, true, 0)
                ON CONFLICT ("勘定科目コード") DO NOTHING
                """);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    @BeforeEach
    void setUpEach() {
        // MyBatis Mapper の取得
        mapper = sqlSessionFactory.openSession(true).getMapper(FinancialDataMapper.class);

        // Repository と Service の初期化
        FinancialDataRepository repository = new FinancialDataAdapter(mapper);
        financialStatementService = new FinancialStatementService(repository);
    }

    @AfterEach
    void cleanup() throws SQLException {
        try (Connection conn = DriverManager.getConnection(
                POSTGRES.getJdbcUrl(),
                POSTGRES.getUsername(),
                POSTGRES.getPassword())) {
            conn.createStatement().execute("DELETE FROM \"日次勘定科目残高\"");
        }
    }

    @Test
    @DisplayName("貸借対照表を生成できる")
    void testGenerateBalanceSheet() throws SQLException {
        // Given: 残高データ
        LocalDate asOfDate = LocalDate.of(2025, 3, 31);
        insertBalanceData(asOfDate);

        // When: 貸借対照表を生成
        BalanceSheet balanceSheet = financialStatementService.generateBalanceSheet(asOfDate);

        // Then: 正しく生成されている
        assertThat(balanceSheet).isNotNull();
        assertThat(balanceSheet.getAsOfDate()).isEqualTo(asOfDate);

        // 資産合計
        assertThat(balanceSheet.getTotalAssets())
                .isEqualByComparingTo(new BigDecimal("1300000")); // 100万 + 20万 + 10万

        // 負債合計
        assertThat(balanceSheet.getTotalLiabilities())
                .isEqualByComparingTo(new BigDecimal("300000")); // 20万 + 10万

        // 純資産合計
        assertThat(balanceSheet.getTotalEquity())
                .isEqualByComparingTo(new BigDecimal("1000000")); // 100万

        // 貸借一致
        assertThat(balanceSheet.getTotalLiabilitiesAndEquity())
                .isEqualByComparingTo(balanceSheet.getTotalAssets());
    }

    @Test
    @DisplayName("損益計算書を生成できる")
    void testGenerateIncomeStatement() throws SQLException {
        // Given: 期間データ
        LocalDate fromDate = LocalDate.of(2025, 1, 1);
        LocalDate toDate = LocalDate.of(2025, 3, 31);
        insertIncomeData(fromDate, toDate);

        // When: 損益計算書を生成
        IncomeStatement incomeStatement = financialStatementService.generateIncomeStatement(fromDate, toDate);

        // Then: 正しく生成されている
        assertThat(incomeStatement).isNotNull();
        assertThat(incomeStatement.getFromDate()).isEqualTo(fromDate);
        assertThat(incomeStatement.getToDate()).isEqualTo(toDate);

        // 売上高
        assertThat(incomeStatement.getTotalRevenues())
                .isEqualByComparingTo(new BigDecimal("1000000")); // 100万

        // 売上総利益
        assertThat(incomeStatement.getGrossProfit())
                .isEqualByComparingTo(new BigDecimal("400000")); // 100万 - 60万

        // 営業利益
        assertThat(incomeStatement.getOperatingIncome())
                .isEqualByComparingTo(new BigDecimal("300000")); // 40万 - 10万

        // 当期純利益
        assertThat(incomeStatement.getNetIncome())
                .isEqualByComparingTo(new BigDecimal("300000")); // 100万 - 70万
    }

    @Test
    @DisplayName("財務指標を計算できる")
    void testCalculateFinancialRatios() throws SQLException {
        // Given: 貸借対照表と損益計算書
        LocalDate asOfDate = LocalDate.of(2025, 3, 31);
        LocalDate fromDate = LocalDate.of(2025, 1, 1);
        LocalDate toDate = LocalDate.of(2025, 3, 31);

        insertBalanceData(asOfDate);
        insertIncomeData(fromDate, toDate);

        BalanceSheet balanceSheet = financialStatementService.generateBalanceSheet(asOfDate);
        IncomeStatement incomeStatement = financialStatementService.generateIncomeStatement(fromDate, toDate);

        // When: 財務指標を計算
        FinancialRatios ratios = financialStatementService.calculateFinancialRatios(
                balanceSheet, incomeStatement);

        // Then: 正しく計算されている
        assertThat(ratios).isNotNull();

        // 流動比率 = 流動資産 / 流動負債 * 100
        // (100万 + 20万 + 10万) / (20万 + 10万) * 100 = 433.33%
        assertThat(ratios.getCurrentRatio())
                .isEqualByComparingTo(new BigDecimal("433.33"));

        // 自己資本比率 = 純資産 / 総資産 * 100
        // 100万 / 130万 * 100 = 76.92%
        assertThat(ratios.getDebtToEquityRatio())
                .isEqualByComparingTo(new BigDecimal("76.92"));

        // 売上総利益率 = 売上総利益 / 売上高 * 100
        // 40万 / 100万 * 100 = 40%
        assertThat(ratios.getGrossProfitMargin())
                .isEqualByComparingTo(new BigDecimal("40.00"));

        // 営業利益率 = 営業利益 / 売上高 * 100
        // 30万 / 100万 * 100 = 30%
        assertThat(ratios.getOperatingProfitMargin())
                .isEqualByComparingTo(new BigDecimal("30.00"));

        // ROA = 当期純利益 / 総資産 * 100
        // 30万 / 130万 * 100 = 23.08%
        assertThat(ratios.getRoa())
                .isEqualByComparingTo(new BigDecimal("23.08"));

        // ROE = 当期純利益 / 純資産 * 100
        // 30万 / 100万 * 100 = 30%
        assertThat(ratios.getRoe())
                .isEqualByComparingTo(new BigDecimal("30.00"));
    }

    @Test
    @DisplayName("残高がゼロの場合でも貸借対照表を生成できる")
    void testGenerateBalanceSheetWithZeroBalance() {
        // Given: 残高データなし
        LocalDate asOfDate = LocalDate.of(2025, 3, 31);

        // When: 貸借対照表を生成
        BalanceSheet balanceSheet = financialStatementService.generateBalanceSheet(asOfDate);

        // Then: 正しく生成されている（全てゼロ）
        assertThat(balanceSheet).isNotNull();
        assertThat(balanceSheet.getTotalAssets()).isEqualByComparingTo(BigDecimal.ZERO);
        assertThat(balanceSheet.getTotalLiabilities()).isEqualByComparingTo(BigDecimal.ZERO);
        assertThat(balanceSheet.getTotalEquity()).isEqualByComparingTo(BigDecimal.ZERO);
    }

    /**
     * テスト用残高データを投入（貸借対照表用）
     */
    private void insertBalanceData(LocalDate asOfDate) throws SQLException {
        try (Connection conn = DriverManager.getConnection(
                POSTGRES.getJdbcUrl(),
                POSTGRES.getUsername(),
                POSTGRES.getPassword())) {
            conn.createStatement().executeUpdate(String.format("""
                INSERT INTO "日次勘定科目残高"
                ("起票日", "勘定科目コード", "補助科目コード", "部門コード", "プロジェクトコード", "決算仕訳フラグ", "借方金額", "貸方金額")
                VALUES
                ('%s', '1110', '', '', '', 0, 1000000, 0),  -- 現金 100万（借方）
                ('%s', '1120', '', '', '', 0, 200000, 0),   -- 普通預金 20万（借方）
                ('%s', '1210', '', '', '', 0, 100000, 0),   -- 売掛金 10万（借方）
                ('%s', '2110', '', '', '', 0, 0, 200000),   -- 買掛金 20万（貸方）
                ('%s', '2120', '', '', '', 0, 0, 100000),   -- 短期借入金 10万（貸方）
                ('%s', '3110', '', '', '', 0, 0, 1000000)   -- 資本金 100万（貸方）
                """, asOfDate, asOfDate, asOfDate, asOfDate, asOfDate, asOfDate));
        }
    }

    /**
     * テスト用残高データを投入（損益計算書用）
     */
    private void insertIncomeData(LocalDate fromDate, LocalDate toDate) throws SQLException {
        try (Connection conn = DriverManager.getConnection(
                POSTGRES.getJdbcUrl(),
                POSTGRES.getUsername(),
                POSTGRES.getPassword())) {
            // 期間中の各日付にデータを投入（簡略化のため開始日のみ）
            conn.createStatement().executeUpdate(String.format("""
                INSERT INTO "日次勘定科目残高"
                ("起票日", "勘定科目コード", "補助科目コード", "部門コード", "プロジェクトコード", "決算仕訳フラグ", "借方金額", "貸方金額")
                VALUES
                ('%s', '4110', '', '', '', 0, 0, 1000000),  -- 売上高 100万（貸方）
                ('%s', '5110', '', '', '', 0, 600000, 0),   -- 売上原価 60万（借方）
                ('%s', '6110', '', '', '', 0, 100000, 0)    -- 販売費 10万（借方）
                """, fromDate, fromDate, fromDate));
        }
    }
}
