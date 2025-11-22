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
import java.time.LocalDate;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * 日次勘定科目残高テーブルのテスト（Red フェーズ）
 */
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
@DisplayName("日次勘定科目残高 - スキーマ検証テスト")
class DailyAccountBalanceTest extends TestDatabaseConfig {

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
            conn.createStatement().execute("DELETE FROM \"日次勘定科目残高\"");
        }
    }

    @Test
    @Order(1)
    @DisplayName("日次残高レコードを登録できる")
    void testInsertDailyBalance() throws SQLException {
        // Given: 2025-01-15 の普通預金の日次残高
        LocalDate entryDate = LocalDate.of(2025, 1, 15);
        String accountCode = "1020";  // 普通預金

        try (Connection conn = DriverManager.getConnection(
                POSTGRES.getJdbcUrl(),
                POSTGRES.getUsername(),
                POSTGRES.getPassword())) {

            // When: 日次残高を登録
            PreparedStatement pstmt = conn.prepareStatement("""
                INSERT INTO "日次勘定科目残高" (
                    "起票日", "勘定科目コード", "補助科目コード", "部門コード",
                    "プロジェクトコード", "決算仕訳フラグ", "借方金額", "貸方金額"
                ) VALUES (?, ?, '', '', '', 0, 100000.00, 0.00)
                """);
            pstmt.setObject(1, entryDate);
            pstmt.setString(2, accountCode);
            pstmt.executeUpdate();

            // Then: データが正しく登録されている
            PreparedStatement selectStmt = conn.prepareStatement("""
                SELECT * FROM "日次勘定科目残高"
                WHERE "起票日" = ?
                  AND "勘定科目コード" = ?
                """);
            selectStmt.setObject(1, entryDate);
            selectStmt.setString(2, accountCode);
            ResultSet rs = selectStmt.executeQuery();

            assertThat(rs.next()).isTrue();
            assertThat(rs.getBigDecimal("借方金額")).isEqualByComparingTo(new BigDecimal("100000.00"));
            assertThat(rs.getBigDecimal("貸方金額")).isEqualByComparingTo(new BigDecimal("0.00"));
        }
    }

    @Test
    @Order(2)
    @DisplayName("複合主キーで一意性が保たれる")
    void testCompositePrimaryKeyUniqueness() throws SQLException {
        // Given: 同じキーで日次残高を登録
        LocalDate entryDate = LocalDate.of(2025, 1, 15);
        String accountCode = "1020";

        try (Connection conn = DriverManager.getConnection(
                POSTGRES.getJdbcUrl(),
                POSTGRES.getUsername(),
                POSTGRES.getPassword())) {

            PreparedStatement pstmt = conn.prepareStatement("""
                INSERT INTO "日次勘定科目残高" (
                    "起票日", "勘定科目コード", "補助科目コード", "部門コード",
                    "プロジェクトコード", "決算仕訳フラグ", "借方金額", "貸方金額"
                ) VALUES (?, ?, '', '', '', 0, 100000.00, 0.00)
                """);
            pstmt.setObject(1, entryDate);
            pstmt.setString(2, accountCode);
            pstmt.executeUpdate();

            // When & Then: 同じキーで2回目の登録を試みるとエラー
            boolean exceptionThrown = false;
            try {
                PreparedStatement pstmt2 = conn.prepareStatement("""
                    INSERT INTO "日次勘定科目残高" (
                        "起票日", "勘定科目コード", "補助科目コード", "部門コード",
                        "プロジェクトコード", "決算仕訳フラグ", "借方金額", "貸方金額"
                    ) VALUES (?, ?, '', '', '', 0, 50000.00, 0.00)
                    """);
                pstmt2.setObject(1, entryDate);
                pstmt2.setString(2, accountCode);
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
    @DisplayName("部門別の残高を管理できる")
    void testDepartmentBalanceManagement() throws SQLException {
        // Given: 売上高の部門別日次残高
        LocalDate entryDate = LocalDate.of(2025, 1, 15);
        String accountCode = "4010";  // 売上高

        try (Connection conn = DriverManager.getConnection(
                POSTGRES.getJdbcUrl(),
                POSTGRES.getUsername(),
                POSTGRES.getPassword())) {

            // When: 部門001と部門002の残高を登録
            PreparedStatement pstmt1 = conn.prepareStatement("""
                INSERT INTO "日次勘定科目残高" (
                    "起票日", "勘定科目コード", "補助科目コード", "部門コード",
                    "プロジェクトコード", "決算仕訳フラグ", "借方金額", "貸方金額"
                ) VALUES (?, ?, '', '001', '', 0, 0.00, 300000.00)
                """);
            pstmt1.setObject(1, entryDate);
            pstmt1.setString(2, accountCode);
            pstmt1.executeUpdate();

            PreparedStatement pstmt2 = conn.prepareStatement("""
                INSERT INTO "日次勘定科目残高" (
                    "起票日", "勘定科目コード", "補助科目コード", "部門コード",
                    "プロジェクトコード", "決算仕訳フラグ", "借方金額", "貸方金額"
                ) VALUES (?, ?, '', '002', '', 0, 0.00, 200000.00)
                """);
            pstmt2.setObject(1, entryDate);
            pstmt2.setString(2, accountCode);
            pstmt2.executeUpdate();

            // Then: 部門別に集計できる
            PreparedStatement selectStmt = conn.prepareStatement("""
                SELECT "部門コード", SUM("貸方金額") as 売上合計
                FROM "日次勘定科目残高"
                WHERE "勘定科目コード" = ?
                GROUP BY "部門コード"
                ORDER BY "部門コード"
                """);
            selectStmt.setString(1, accountCode);
            ResultSet rs = selectStmt.executeQuery();

            assertThat(rs.next()).isTrue();
            assertThat(rs.getString("部門コード")).isEqualTo("001");
            assertThat(rs.getBigDecimal("売上合計")).isEqualByComparingTo(new BigDecimal("300000.00"));

            assertThat(rs.next()).isTrue();
            assertThat(rs.getString("部門コード")).isEqualTo("002");
            assertThat(rs.getBigDecimal("売上合計")).isEqualByComparingTo(new BigDecimal("200000.00"));
        }
    }

    @Test
    @Order(4)
    @DisplayName("プロジェクト別の残高を管理できる")
    void testProjectBalanceManagement() throws SQLException {
        // Given: プロジェクト別の残高
        LocalDate entryDate = LocalDate.of(2025, 1, 15);
        String accountCode = "4010";  // 売上高

        try (Connection conn = DriverManager.getConnection(
                POSTGRES.getJdbcUrl(),
                POSTGRES.getUsername(),
                POSTGRES.getPassword())) {

            // When: プロジェクトP001とP002の残高を登録
            PreparedStatement pstmt1 = conn.prepareStatement("""
                INSERT INTO "日次勘定科目残高" (
                    "起票日", "勘定科目コード", "補助科目コード", "部門コード",
                    "プロジェクトコード", "決算仕訳フラグ", "借方金額", "貸方金額"
                ) VALUES (?, ?, '', '', 'P001', 0, 0.00, 150000.00)
                """);
            pstmt1.setObject(1, entryDate);
            pstmt1.setString(2, accountCode);
            pstmt1.executeUpdate();

            PreparedStatement pstmt2 = conn.prepareStatement("""
                INSERT INTO "日次勘定科目残高" (
                    "起票日", "勘定科目コード", "補助科目コード", "部門コード",
                    "プロジェクトコード", "決算仕訳フラグ", "借方金額", "貸方金額"
                ) VALUES (?, ?, '', '', 'P002', 0, 0.00, 250000.00)
                """);
            pstmt2.setObject(1, entryDate);
            pstmt2.setString(2, accountCode);
            pstmt2.executeUpdate();

            // Then: プロジェクト別に集計できる
            PreparedStatement selectStmt = conn.prepareStatement("""
                SELECT "プロジェクトコード", SUM("貸方金額") as 売上合計
                FROM "日次勘定科目残高"
                WHERE "勘定科目コード" = ?
                GROUP BY "プロジェクトコード"
                ORDER BY "プロジェクトコード"
                """);
            selectStmt.setString(1, accountCode);
            ResultSet rs = selectStmt.executeQuery();

            assertThat(rs.next()).isTrue();
            assertThat(rs.getString("プロジェクトコード")).isEqualTo("P001");
            assertThat(rs.getBigDecimal("売上合計")).isEqualByComparingTo(new BigDecimal("150000.00"));

            assertThat(rs.next()).isTrue();
            assertThat(rs.getString("プロジェクトコード")).isEqualTo("P002");
            assertThat(rs.getBigDecimal("売上合計")).isEqualByComparingTo(new BigDecimal("250000.00"));
        }
    }

    @Test
    @Order(5)
    @DisplayName("補助科目別の残高を管理できる")
    void testSubsidiaryAccountBalanceManagement() throws SQLException {
        // Given: 売掛金の補助科目（得意先）別残高
        LocalDate entryDate = LocalDate.of(2025, 1, 15);
        String accountCode = "1130";  // 売掛金

        try (Connection conn = DriverManager.getConnection(
                POSTGRES.getJdbcUrl(),
                POSTGRES.getUsername(),
                POSTGRES.getPassword())) {

            // When: 得意先A001とA002の残高を登録
            PreparedStatement pstmt1 = conn.prepareStatement("""
                INSERT INTO "日次勘定科目残高" (
                    "起票日", "勘定科目コード", "補助科目コード", "部門コード",
                    "プロジェクトコード", "決算仕訳フラグ", "借方金額", "貸方金額"
                ) VALUES (?, ?, 'A001', '', '', 0, 500000.00, 0.00)
                """);
            pstmt1.setObject(1, entryDate);
            pstmt1.setString(2, accountCode);
            pstmt1.executeUpdate();

            PreparedStatement pstmt2 = conn.prepareStatement("""
                INSERT INTO "日次勘定科目残高" (
                    "起票日", "勘定科目コード", "補助科目コード", "部門コード",
                    "プロジェクトコード", "決算仕訳フラグ", "借方金額", "貸方金額"
                ) VALUES (?, ?, 'A002', '', '', 0, 300000.00, 0.00)
                """);
            pstmt2.setObject(1, entryDate);
            pstmt2.setString(2, accountCode);
            pstmt2.executeUpdate();

            // Then: 補助科目別に集計できる
            PreparedStatement selectStmt = conn.prepareStatement("""
                SELECT "補助科目コード", SUM("借方金額") as 売掛金合計
                FROM "日次勘定科目残高"
                WHERE "勘定科目コード" = ?
                GROUP BY "補助科目コード"
                ORDER BY "補助科目コード"
                """);
            selectStmt.setString(1, accountCode);
            ResultSet rs = selectStmt.executeQuery();

            assertThat(rs.next()).isTrue();
            assertThat(rs.getString("補助科目コード")).isEqualTo("A001");
            assertThat(rs.getBigDecimal("売掛金合計")).isEqualByComparingTo(new BigDecimal("500000.00"));

            assertThat(rs.next()).isTrue();
            assertThat(rs.getString("補助科目コード")).isEqualTo("A002");
            assertThat(rs.getBigDecimal("売掛金合計")).isEqualByComparingTo(new BigDecimal("300000.00"));
        }
    }

    @Test
    @Order(6)
    @DisplayName("決算仕訳フラグで通常仕訳と決算仕訳を分けて管理できる")
    void testSettlementJournalFlag() throws SQLException {
        // Given: 通常仕訳と決算仕訳の残高
        LocalDate entryDate = LocalDate.of(2025, 3, 31);
        String accountCode = "5110";  // 仕入

        try (Connection conn = DriverManager.getConnection(
                POSTGRES.getJdbcUrl(),
                POSTGRES.getUsername(),
                POSTGRES.getPassword())) {

            // When: 通常仕訳と決算仕訳の残高を登録
            PreparedStatement pstmt1 = conn.prepareStatement("""
                INSERT INTO "日次勘定科目残高" (
                    "起票日", "勘定科目コード", "補助科目コード", "部門コード",
                    "プロジェクトコード", "決算仕訳フラグ", "借方金額", "貸方金額"
                ) VALUES (?, ?, '', '', '', 0, 1000000.00, 0.00)
                """);
            pstmt1.setObject(1, entryDate);
            pstmt1.setString(2, accountCode);
            pstmt1.executeUpdate();

            PreparedStatement pstmt2 = conn.prepareStatement("""
                INSERT INTO "日次勘定科目残高" (
                    "起票日", "勘定科目コード", "補助科目コード", "部門コード",
                    "プロジェクトコード", "決算仕訳フラグ", "借方金額", "貸方金額"
                ) VALUES (?, ?, '', '', '', 1, 50000.00, 0.00)
                """);
            pstmt2.setObject(1, entryDate);
            pstmt2.setString(2, accountCode);
            pstmt2.executeUpdate();

            // Then: 決算仕訳フラグで区別できる
            // 通常仕訳のみの合計
            PreparedStatement normalStmt = conn.prepareStatement("""
                SELECT SUM("借方金額") as total
                FROM "日次勘定科目残高"
                WHERE "勘定科目コード" = ? AND "決算仕訳フラグ" = 0
                """);
            normalStmt.setString(1, accountCode);
            ResultSet normalRs = normalStmt.executeQuery();
            normalRs.next();
            BigDecimal normalTotal = normalRs.getBigDecimal("total");

            // 決算仕訳のみの合計
            PreparedStatement settlementStmt = conn.prepareStatement("""
                SELECT SUM("借方金額") as total
                FROM "日次勘定科目残高"
                WHERE "勘定科目コード" = ? AND "決算仕訳フラグ" = 1
                """);
            settlementStmt.setString(1, accountCode);
            ResultSet settlementRs = settlementStmt.executeQuery();
            settlementRs.next();
            BigDecimal settlementTotal = settlementRs.getBigDecimal("total");

            assertThat(normalTotal).isEqualByComparingTo(new BigDecimal("1000000.00"));
            assertThat(settlementTotal).isEqualByComparingTo(new BigDecimal("50000.00"));
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
                ('1130', '売掛金', '資産'::account_type, false, true, 0),
                ('4010', '売上高', '収益'::account_type, false, true, 0),
                ('5110', '仕入', '費用'::account_type, false, true, 0)
                ON CONFLICT ("勘定科目コード") DO NOTHING
                """);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }
}
