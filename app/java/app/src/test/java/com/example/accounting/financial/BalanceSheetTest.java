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
 * 貸借対照表生成のテスト（Red フェーズ）
 */
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
@DisplayName("貸借対照表 - 生成テスト")
class BalanceSheetTest extends TestDatabaseConfig {

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
    @DisplayName("貸借対照表を生成できる")
    void testGenerateBalanceSheet() throws SQLException {
        // Given: 2024-01-31時点のデータ
        LocalDate asOfDate = LocalDate.of(2024, 1, 31);
        insertTestBalanceData(asOfDate);

        financialStatementService = new FinancialStatementService(
                postgres.getJdbcUrl(),
                postgres.getUsername(),
                postgres.getPassword()
        );

        // When: 貸借対照表を生成
        BalanceSheet balanceSheet = financialStatementService.generateBalanceSheet(asOfDate);

        // Then: 貸借対照表のプロパティが存在する
        assertThat(balanceSheet).isNotNull();
        assertThat(balanceSheet.getAsOfDate()).isEqualTo(asOfDate);
        assertThat(balanceSheet.getAssets()).isNotEmpty();
        assertThat(balanceSheet.getLiabilities()).isNotEmpty();
        assertThat(balanceSheet.getEquity()).isNotEmpty();
        assertThat(balanceSheet.getTotalAssets()).isGreaterThan(BigDecimal.ZERO);
        assertThat(balanceSheet.getTotalLiabilities()).isGreaterThan(BigDecimal.ZERO);
        assertThat(balanceSheet.getTotalEquity()).isGreaterThan(BigDecimal.ZERO);
        assertThat(balanceSheet.getTotalLiabilitiesAndEquity()).isGreaterThan(BigDecimal.ZERO);
    }

    @Test
    @Order(2)
    @DisplayName("貸借平均の原則が成立している")
    void testBalancingPrinciple() throws SQLException {
        // Given: 2024-01-31時点のデータ
        LocalDate asOfDate = LocalDate.of(2024, 1, 31);
        insertTestBalanceData(asOfDate);

        financialStatementService = new FinancialStatementService(
                postgres.getJdbcUrl(),
                postgres.getUsername(),
                postgres.getPassword()
        );

        // When: 貸借対照表を生成
        BalanceSheet balanceSheet = financialStatementService.generateBalanceSheet(asOfDate);

        // Then: 資産 = 負債 + 純資産
        BigDecimal expectedTotal = balanceSheet.getTotalLiabilities()
                .add(balanceSheet.getTotalEquity());

        assertThat(balanceSheet.getTotalAssets())
                .isEqualByComparingTo(expectedTotal);
        assertThat(balanceSheet.getTotalAssets())
                .isEqualByComparingTo(balanceSheet.getTotalLiabilitiesAndEquity());

        // 具体的な金額の検証
        // 資産: 普通預金8,000,000 + 建物2,000,000 = 10,000,000
        assertThat(balanceSheet.getTotalAssets())
                .isEqualByComparingTo(new BigDecimal("10000000"));

        // 負債: 買掛金500,000 + 長期借入金4,500,000 = 5,000,000
        assertThat(balanceSheet.getTotalLiabilities())
                .isEqualByComparingTo(new BigDecimal("5000000"));

        // 純資産: 資本金5,000,000
        assertThat(balanceSheet.getTotalEquity())
                .isEqualByComparingTo(new BigDecimal("5000000"));
    }

    @Test
    @Order(3)
    @DisplayName("資産項目が正しく分類されている")
    void testAssetClassification() throws SQLException {
        // Given: 2024-01-31時点のデータ
        LocalDate asOfDate = LocalDate.of(2024, 1, 31);
        insertTestBalanceData(asOfDate);

        financialStatementService = new FinancialStatementService(
                postgres.getJdbcUrl(),
                postgres.getUsername(),
                postgres.getPassword()
        );

        // When: 貸借対照表を生成
        BalanceSheet balanceSheet = financialStatementService.generateBalanceSheet(asOfDate);

        // Then: 資産項目が存在する
        List<BalanceSheetItem> assets = balanceSheet.getAssets();
        assertThat(assets).hasSize(2);

        // 流動資産（11で始まる勘定科目）
        List<BalanceSheetItem> currentAssets = assets.stream()
                .filter(asset -> asset.getAccountCode().startsWith("11"))
                .toList();
        assertThat(currentAssets).hasSize(1);
        assertThat(currentAssets.get(0).getAccountCode()).isEqualTo("1110");
        assertThat(currentAssets.get(0).getAccountName()).isEqualTo("普通預金");
        assertThat(currentAssets.get(0).getBalance())
                .isEqualByComparingTo(new BigDecimal("8000000"));

        // 固定資産（14で始まる勘定科目）
        List<BalanceSheetItem> fixedAssets = assets.stream()
                .filter(asset -> asset.getAccountCode().startsWith("14"))
                .toList();
        assertThat(fixedAssets).hasSize(1);
        assertThat(fixedAssets.get(0).getAccountCode()).isEqualTo("1410");
        assertThat(fixedAssets.get(0).getAccountName()).isEqualTo("建物");
        assertThat(fixedAssets.get(0).getBalance())
                .isEqualByComparingTo(new BigDecimal("2000000"));
    }

    @Test
    @Order(4)
    @DisplayName("負債項目が正しく分類されている")
    void testLiabilityClassification() throws SQLException {
        // Given: 2024-01-31時点のデータ
        LocalDate asOfDate = LocalDate.of(2024, 1, 31);
        insertTestBalanceData(asOfDate);

        financialStatementService = new FinancialStatementService(
                postgres.getJdbcUrl(),
                postgres.getUsername(),
                postgres.getPassword()
        );

        // When: 貸借対照表を生成
        BalanceSheet balanceSheet = financialStatementService.generateBalanceSheet(asOfDate);

        // Then: 負債項目が存在する
        List<BalanceSheetItem> liabilities = balanceSheet.getLiabilities();
        assertThat(liabilities).hasSize(2);

        // 流動負債（21で始まる勘定科目）
        List<BalanceSheetItem> currentLiabilities = liabilities.stream()
                .filter(liability -> liability.getAccountCode().startsWith("21"))
                .toList();
        assertThat(currentLiabilities).hasSize(1);
        assertThat(currentLiabilities.get(0).getAccountCode()).isEqualTo("2110");
        assertThat(currentLiabilities.get(0).getAccountName()).isEqualTo("買掛金");
        assertThat(currentLiabilities.get(0).getBalance())
                .isEqualByComparingTo(new BigDecimal("500000"));

        // 固定負債（25で始まる勘定科目）
        List<BalanceSheetItem> longTermLiabilities = liabilities.stream()
                .filter(liability -> liability.getAccountCode().startsWith("25"))
                .toList();
        assertThat(longTermLiabilities).hasSize(1);
        assertThat(longTermLiabilities.get(0).getAccountCode()).isEqualTo("2510");
        assertThat(longTermLiabilities.get(0).getAccountName()).isEqualTo("長期借入金");
        assertThat(longTermLiabilities.get(0).getBalance())
                .isEqualByComparingTo(new BigDecimal("4500000"));
    }

    @Test
    @Order(5)
    @DisplayName("構成比率が計算されている")
    void testCompositionRatio() throws SQLException {
        // Given: 2024-01-31時点のデータ
        LocalDate asOfDate = LocalDate.of(2024, 1, 31);
        insertTestBalanceData(asOfDate);

        financialStatementService = new FinancialStatementService(
                postgres.getJdbcUrl(),
                postgres.getUsername(),
                postgres.getPassword()
        );

        // When: 貸借対照表を生成
        BalanceSheet balanceSheet = financialStatementService.generateBalanceSheet(asOfDate);

        // Then: 各項目の構成比率が存在し、0〜100の範囲内
        for (BalanceSheetItem asset : balanceSheet.getAssets()) {
            assertThat(asset.getRatio()).isNotNull();
            assertThat(asset.getRatio()).isBetween(BigDecimal.ZERO, new BigDecimal("100"));
        }

        for (BalanceSheetItem liability : balanceSheet.getLiabilities()) {
            assertThat(liability.getRatio()).isNotNull();
            assertThat(liability.getRatio()).isBetween(BigDecimal.ZERO, new BigDecimal("100"));
        }

        for (BalanceSheetItem eq : balanceSheet.getEquity()) {
            assertThat(eq.getRatio()).isNotNull();
            assertThat(eq.getRatio()).isBetween(BigDecimal.ZERO, new BigDecimal("100"));
        }

        // 普通預金の構成比率：8,000,000 / 10,000,000 = 80%
        BalanceSheetItem cash = balanceSheet.getAssets().stream()
                .filter(a -> a.getAccountCode().equals("1110"))
                .findFirst()
                .orElseThrow();
        assertThat(cash.getRatio()).isEqualByComparingTo(new BigDecimal("80.00"));
    }

    /**
     * テスト用勘定科目を登録
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
                ('1110', '普通預金', '資産'::account_type, 'B', '1', false, true, 0),
                ('1410', '建物', '資産'::account_type, 'B', '1', false, true, 0),
                ('2110', '買掛金', '負債'::account_type, 'B', '2', false, true, 0),
                ('2510', '長期借入金', '負債'::account_type, 'B', '2', false, true, 0),
                ('3110', '資本金', '純資産'::account_type, 'B', '3', false, true, 0)
                ON CONFLICT ("勘定科目コード") DO NOTHING
                """);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * テスト用残高データを登録
     */
    private void insertTestBalanceData(LocalDate asOfDate) throws SQLException {
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
}
