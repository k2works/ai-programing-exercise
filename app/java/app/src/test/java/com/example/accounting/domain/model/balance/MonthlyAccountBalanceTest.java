package com.example.accounting.domain.model.balance;

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
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * 月次勘定科目残高テーブルのテスト
 */
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
@DisplayName("月次勘定科目残高 - スキーマ検証テスト")
class MonthlyAccountBalanceTest extends TestDatabaseConfig {

    @BeforeAll
    static void setUp() throws Exception {
        // テスト用勘定科目を登録
        insertTestAccounts();
    }

    @AfterEach
    void cleanup() throws SQLException {
        try (Connection conn = DriverManager.getConnection(
                POSTGRES.getJdbcUrl(),
                POSTGRES.getUsername(),
                POSTGRES.getPassword())) {
            conn.createStatement().execute("DELETE FROM \"月次勘定科目残高\"");
        }
    }

    @Test
    @Order(1)
    @DisplayName("月次残高レコードを登録できる")
    void testInsertMonthlyBalance() throws SQLException {
        // Given: 2025年1月度の普通預金の月次残高
        int fiscalYear = 2025;
        int month = 1;
        String accountCode = "1020";  // 普通預金

        try (Connection conn = DriverManager.getConnection(
                POSTGRES.getJdbcUrl(),
                POSTGRES.getUsername(),
                POSTGRES.getPassword())) {

            // When: 月次残高を登録
            PreparedStatement pstmt = conn.prepareStatement("""
                INSERT INTO "月次勘定科目残高" (
                    "決算期", "月度", "勘定科目コード", "補助科目コード", "部門コード",
                    "プロジェクトコード", "決算仕訳フラグ", "月初残高", "借方金額", "貸方金額", "月末残高"
                ) VALUES (?, ?, ?, '', '', '', 0, 1000000.00, 500000.00, 300000.00, 1200000.00)
                """);
            pstmt.setInt(1, fiscalYear);
            pstmt.setInt(2, month);
            pstmt.setString(3, accountCode);
            pstmt.executeUpdate();

            // Then: データが正しく登録されている
            PreparedStatement selectStmt = conn.prepareStatement("""
                SELECT * FROM "月次勘定科目残高"
                WHERE "決算期" = ? AND "月度" = ? AND "勘定科目コード" = ?
                """);
            selectStmt.setInt(1, fiscalYear);
            selectStmt.setInt(2, month);
            selectStmt.setString(3, accountCode);
            ResultSet rs = selectStmt.executeQuery();

            assertThat(rs.next()).isTrue();
            assertThat(rs.getBigDecimal("月初残高")).isEqualByComparingTo(new BigDecimal("1000000.00"));
            assertThat(rs.getBigDecimal("借方金額")).isEqualByComparingTo(new BigDecimal("500000.00"));
            assertThat(rs.getBigDecimal("貸方金額")).isEqualByComparingTo(new BigDecimal("300000.00"));
            assertThat(rs.getBigDecimal("月末残高")).isEqualByComparingTo(new BigDecimal("1200000.00"));
        }
    }

    @Test
    @Order(2)
    @DisplayName("複合主キーで一意性が保たれる")
    void testCompositePrimaryKeyUniqueness() throws SQLException {
        // Given: 同じキーで月次残高を登録
        int fiscalYear = 2025;
        int month = 1;
        String accountCode = "1020";

        try (Connection conn = DriverManager.getConnection(
                POSTGRES.getJdbcUrl(),
                POSTGRES.getUsername(),
                POSTGRES.getPassword())) {

            PreparedStatement pstmt = conn.prepareStatement("""
                INSERT INTO "月次勘定科目残高" (
                    "決算期", "月度", "勘定科目コード", "補助科目コード", "部門コード",
                    "プロジェクトコード", "決算仕訳フラグ", "月初残高", "借方金額", "貸方金額", "月末残高"
                ) VALUES (?, ?, ?, '', '', '', 0, 1000000.00, 500000.00, 300000.00, 1200000.00)
                """);
            pstmt.setInt(1, fiscalYear);
            pstmt.setInt(2, month);
            pstmt.setString(3, accountCode);
            pstmt.executeUpdate();

            // When & Then: 同じキーで2回目の登録を試みるとエラー
            boolean exceptionThrown = false;
            try {
                PreparedStatement pstmt2 = conn.prepareStatement("""
                    INSERT INTO "月次勘定科目残高" (
                        "決算期", "月度", "勘定科目コード", "補助科目コード", "部門コード",
                        "プロジェクトコード", "決算仕訳フラグ", "月初残高", "借方金額", "貸方金額", "月末残高"
                    ) VALUES (?, ?, ?, '', '', '', 0, 2000000.00, 100000.00, 50000.00, 2050000.00)
                    """);
                pstmt2.setInt(1, fiscalYear);
                pstmt2.setInt(2, month);
                pstmt2.setString(3, accountCode);
                pstmt2.executeUpdate();
            } catch (SQLException e) {
                exceptionThrown = true;
                assertThat(e.getMessage()).contains("duplicate key value violates unique constraint");
            }
            assertThat(exceptionThrown).isTrue();
        }
    }

    @Test
    @Order(3)
    @DisplayName("月度のCHECK制約が機能する")
    void testMonthRangeCheck() throws SQLException {
        // Given: 不正な月度（0または13）でレコードを登録しようとする
        int fiscalYear = 2025;
        String accountCode = "1020";

        try (Connection conn = DriverManager.getConnection(
                POSTGRES.getJdbcUrl(),
                POSTGRES.getUsername(),
                POSTGRES.getPassword())) {

            // When & Then: 月度=0で登録を試みるとエラー
            boolean exceptionThrown = false;
            try {
                PreparedStatement pstmt = conn.prepareStatement("""
                    INSERT INTO "月次勘定科目残高" (
                        "決算期", "月度", "勘定科目コード", "補助科目コード", "部門コード",
                        "プロジェクトコード", "決算仕訳フラグ", "月初残高", "借方金額", "貸方金額", "月末残高"
                    ) VALUES (?, ?, ?, '', '', '', 0, 0, 0, 0, 0)
                    """);
                pstmt.setInt(1, fiscalYear);
                pstmt.setInt(2, 0);  // 不正な月度
                pstmt.setString(3, accountCode);
                pstmt.executeUpdate();
            } catch (SQLException e) {
                exceptionThrown = true;
                assertThat(e.getMessage()).contains("check_月次残高_月度範囲");
            }
            assertThat(exceptionThrown).isTrue();

            // When & Then: 月度=13で登録を試みるとエラー
            exceptionThrown = false;
            try {
                PreparedStatement pstmt = conn.prepareStatement("""
                    INSERT INTO "月次勘定科目残高" (
                        "決算期", "月度", "勘定科目コード", "補助科目コード", "部門コード",
                        "プロジェクトコード", "決算仕訳フラグ", "月初残高", "借方金額", "貸方金額", "月末残高"
                    ) VALUES (?, ?, ?, '', '', '', 0, 0, 0, 0, 0)
                    """);
                pstmt.setInt(1, fiscalYear);
                pstmt.setInt(2, 13);  // 不正な月度
                pstmt.setString(3, accountCode);
                pstmt.executeUpdate();
            } catch (SQLException e) {
                exceptionThrown = true;
                assertThat(e.getMessage()).contains("check_月次残高_月度範囲");
            }
            assertThat(exceptionThrown).isTrue();
        }
    }

    @Test
    @Order(4)
    @DisplayName("部門別の月次残高を管理できる")
    void testDepartmentMonthlyBalance() throws SQLException {
        // Given: 売上高の部門別月次残高
        int fiscalYear = 2025;
        int month = 1;
        String accountCode = "4010";  // 売上高

        try (Connection conn = DriverManager.getConnection(
                POSTGRES.getJdbcUrl(),
                POSTGRES.getUsername(),
                POSTGRES.getPassword())) {

            // When: 部門001と部門002の残高を登録
            PreparedStatement pstmt1 = conn.prepareStatement("""
                INSERT INTO "月次勘定科目残高" (
                    "決算期", "月度", "勘定科目コード", "補助科目コード", "部門コード",
                    "プロジェクトコード", "決算仕訳フラグ", "月初残高", "借方金額", "貸方金額", "月末残高"
                ) VALUES (?, ?, ?, '', '001', '', 0, 0.00, 0.00, 3000000.00, -3000000.00)
                """);
            pstmt1.setInt(1, fiscalYear);
            pstmt1.setInt(2, month);
            pstmt1.setString(3, accountCode);
            pstmt1.executeUpdate();

            PreparedStatement pstmt2 = conn.prepareStatement("""
                INSERT INTO "月次勘定科目残高" (
                    "決算期", "月度", "勘定科目コード", "補助科目コード", "部門コード",
                    "プロジェクトコード", "決算仕訳フラグ", "月初残高", "借方金額", "貸方金額", "月末残高"
                ) VALUES (?, ?, ?, '', '002', '', 0, 0.00, 0.00, 2000000.00, -2000000.00)
                """);
            pstmt2.setInt(1, fiscalYear);
            pstmt2.setInt(2, month);
            pstmt2.setString(3, accountCode);
            pstmt2.executeUpdate();

            // Then: 部門別に集計できる
            PreparedStatement selectStmt = conn.prepareStatement("""
                SELECT "部門コード", SUM("貸方金額") as 売上合計
                FROM "月次勘定科目残高"
                WHERE "勘定科目コード" = ? AND "決算期" = ? AND "月度" = ?
                GROUP BY "部門コード"
                ORDER BY "部門コード"
                """);
            selectStmt.setString(1, accountCode);
            selectStmt.setInt(2, fiscalYear);
            selectStmt.setInt(3, month);
            ResultSet rs = selectStmt.executeQuery();

            assertThat(rs.next()).isTrue();
            assertThat(rs.getString("部門コード")).isEqualTo("001");
            assertThat(rs.getBigDecimal("売上合計")).isEqualByComparingTo(new BigDecimal("3000000.00"));

            assertThat(rs.next()).isTrue();
            assertThat(rs.getString("部門コード")).isEqualTo("002");
            assertThat(rs.getBigDecimal("売上合計")).isEqualByComparingTo(new BigDecimal("2000000.00"));
        }
    }

    @Test
    @Order(5)
    @DisplayName("年度をまたいだ月次残高を管理できる")
    void testMultiFiscalYearBalance() throws SQLException {
        // Given: 2024年度と2025年度の月次残高
        String accountCode = "1020";  // 普通預金

        try (Connection conn = DriverManager.getConnection(
                POSTGRES.getJdbcUrl(),
                POSTGRES.getUsername(),
                POSTGRES.getPassword())) {

            // When: 2024年12月と2025年1月の残高を登録
            PreparedStatement pstmt1 = conn.prepareStatement("""
                INSERT INTO "月次勘定科目残高" (
                    "決算期", "月度", "勘定科目コード", "補助科目コード", "部門コード",
                    "プロジェクトコード", "決算仕訳フラグ", "月初残高", "借方金額", "貸方金額", "月末残高"
                ) VALUES (?, ?, ?, '', '', '', 0, 1000000.00, 200000.00, 100000.00, 1100000.00)
                """);
            pstmt1.setInt(1, 2024);
            pstmt1.setInt(2, 12);
            pstmt1.setString(3, accountCode);
            pstmt1.executeUpdate();

            PreparedStatement pstmt2 = conn.prepareStatement("""
                INSERT INTO "月次勘定科目残高" (
                    "決算期", "月度", "勘定科目コード", "補助科目コード", "部門コード",
                    "プロジェクトコード", "決算仕訳フラグ", "月初残高", "借方金額", "貸方金額", "月末残高"
                ) VALUES (?, ?, ?, '', '', '', 0, 1100000.00, 300000.00, 150000.00, 1250000.00)
                """);
            pstmt2.setInt(1, 2025);
            pstmt2.setInt(2, 1);
            pstmt2.setString(3, accountCode);
            pstmt2.executeUpdate();

            // Then: 年度別に取得できる
            PreparedStatement selectStmt = conn.prepareStatement("""
                SELECT "決算期", "月度", "月末残高"
                FROM "月次勘定科目残高"
                WHERE "勘定科目コード" = ?
                ORDER BY "決算期", "月度"
                """);
            selectStmt.setString(1, accountCode);
            ResultSet rs = selectStmt.executeQuery();

            assertThat(rs.next()).isTrue();
            assertThat(rs.getInt("決算期")).isEqualTo(2024);
            assertThat(rs.getInt("月度")).isEqualTo(12);
            assertThat(rs.getBigDecimal("月末残高")).isEqualByComparingTo(new BigDecimal("1100000.00"));

            assertThat(rs.next()).isTrue();
            assertThat(rs.getInt("決算期")).isEqualTo(2025);
            assertThat(rs.getInt("月度")).isEqualTo(1);
            assertThat(rs.getBigDecimal("月末残高")).isEqualByComparingTo(new BigDecimal("1250000.00"));
        }
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
                INSERT INTO "勘定科目マスタ" ("勘定科目コード", "勘定科目名", "勘定科目種別", "合計科目", "集計対象", "残高")
                VALUES
                ('1020', '普通預金', '資産'::account_type, false, true, 0),
                ('4010', '売上高', '収益'::account_type, false, true, 0)
                ON CONFLICT ("勘定科目コード") DO NOTHING
                """);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }
}
