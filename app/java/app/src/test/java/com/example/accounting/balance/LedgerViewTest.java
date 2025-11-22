package com.example.accounting.balance;

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
 * 総勘定元帳と試算表ビューのテスト
 */
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
@DisplayName("総勘定元帳・試算表 - ビューテスト")
class LedgerViewTest extends TestDatabaseConfig {

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
    @DisplayName("総勘定元帳ビューから累積残高を取得できる")
    void testGeneralLedgerView() throws SQLException {
        // Given: 複数日の取引データを登録
        LocalDate date1 = LocalDate.of(2025, 1, 10);
        LocalDate date2 = LocalDate.of(2025, 1, 15);
        LocalDate date3 = LocalDate.of(2025, 1, 20);
        String accountCode = "1020";  // 普通預金

        try (Connection conn = DriverManager.getConnection(
                POSTGRES.getJdbcUrl(),
                POSTGRES.getUsername(),
                POSTGRES.getPassword())) {

            // 1/10: 借方 100,000
            PreparedStatement pstmt1 = conn.prepareStatement("""
                INSERT INTO "日次勘定科目残高" (
                    "起票日", "勘定科目コード", "補助科目コード", "部門コード",
                    "プロジェクトコード", "決算仕訳フラグ", "借方金額", "貸方金額"
                ) VALUES (?, ?, '', '', '', 0, 100000.00, 0.00)
                """);
            pstmt1.setObject(1, date1);
            pstmt1.setString(2, accountCode);
            pstmt1.executeUpdate();

            // 1/15: 貸方 30,000
            PreparedStatement pstmt2 = conn.prepareStatement("""
                INSERT INTO "日次勘定科目残高" (
                    "起票日", "勘定科目コード", "補助科目コード", "部門コード",
                    "プロジェクトコード", "決算仕訳フラグ", "借方金額", "貸方金額"
                ) VALUES (?, ?, '', '', '', 0, 0.00, 30000.00)
                """);
            pstmt2.setObject(1, date2);
            pstmt2.setString(2, accountCode);
            pstmt2.executeUpdate();

            // 1/20: 借方 50,000
            PreparedStatement pstmt3 = conn.prepareStatement("""
                INSERT INTO "日次勘定科目残高" (
                    "起票日", "勘定科目コード", "補助科目コード", "部門コード",
                    "プロジェクトコード", "決算仕訳フラグ", "借方金額", "貸方金額"
                ) VALUES (?, ?, '', '', '', 0, 50000.00, 0.00)
                """);
            pstmt3.setObject(1, date3);
            pstmt3.setString(2, accountCode);
            pstmt3.executeUpdate();

            // When: 総勘定元帳ビューを参照
            PreparedStatement selectStmt = conn.prepareStatement("""
                SELECT * FROM "総勘定元帳"
                WHERE account_code = ?
                ORDER BY entry_date
                """);
            selectStmt.setString(1, accountCode);
            ResultSet rs = selectStmt.executeQuery();

            // Then: 累積残高が正しく計算されている
            assertThat(rs.next()).isTrue();
            assertThat(rs.getDate("entry_date").toLocalDate()).isEqualTo(date1);
            assertThat(rs.getBigDecimal("debit_amount")).isEqualByComparingTo(new BigDecimal("100000.00"));
            assertThat(rs.getBigDecimal("balance")).isEqualByComparingTo(new BigDecimal("100000.00"));

            assertThat(rs.next()).isTrue();
            assertThat(rs.getDate("entry_date").toLocalDate()).isEqualTo(date2);
            assertThat(rs.getBigDecimal("credit_amount")).isEqualByComparingTo(new BigDecimal("30000.00"));
            // 100000 - 30000 = 70000
            assertThat(rs.getBigDecimal("balance")).isEqualByComparingTo(new BigDecimal("70000.00"));

            assertThat(rs.next()).isTrue();
            assertThat(rs.getDate("entry_date").toLocalDate()).isEqualTo(date3);
            assertThat(rs.getBigDecimal("debit_amount")).isEqualByComparingTo(new BigDecimal("50000.00"));
            // 70000 + 50000 = 120000
            assertThat(rs.getBigDecimal("balance")).isEqualByComparingTo(new BigDecimal("120000.00"));
        }
    }

    @Test
    @Order(2)
    @DisplayName("試算表ビューから全勘定科目の残高を取得できる")
    void testTrialBalanceView() throws SQLException {
        // Given: 複数勘定科目の残高データを登録
        LocalDate date = LocalDate.of(2025, 1, 15);

        try (Connection conn = DriverManager.getConnection(
                POSTGRES.getJdbcUrl(),
                POSTGRES.getUsername(),
                POSTGRES.getPassword())) {

            // 普通預金（資産・借方）: 借方 500,000、貸方 200,000
            PreparedStatement pstmt1 = conn.prepareStatement("""
                INSERT INTO "日次勘定科目残高" (
                    "起票日", "勘定科目コード", "補助科目コード", "部門コード",
                    "プロジェクトコード", "決算仕訳フラグ", "借方金額", "貸方金額"
                ) VALUES (?, ?, '', '', '', 0, 500000.00, 200000.00)
                """);
            pstmt1.setObject(1, date);
            pstmt1.setString(2, "1020");
            pstmt1.executeUpdate();

            // 売上高（収益・貸方）: 借方 0、貸方 1,000,000
            PreparedStatement pstmt2 = conn.prepareStatement("""
                INSERT INTO "日次勘定科目残高" (
                    "起票日", "勘定科目コード", "補助科目コード", "部門コード",
                    "プロジェクトコード", "決算仕訳フラグ", "借方金額", "貸方金額"
                ) VALUES (?, ?, '', '', '', 0, 0.00, 1000000.00)
                """);
            pstmt2.setObject(1, date);
            pstmt2.setString(2, "4010");
            pstmt2.executeUpdate();

            // 仕入（費用・借方）: 借方 600,000、貸方 0
            PreparedStatement pstmt3 = conn.prepareStatement("""
                INSERT INTO "日次勘定科目残高" (
                    "起票日", "勘定科目コード", "補助科目コード", "部門コード",
                    "プロジェクトコード", "決算仕訳フラグ", "借方金額", "貸方金額"
                ) VALUES (?, ?, '', '', '', 0, 600000.00, 0.00)
                """);
            pstmt3.setObject(1, date);
            pstmt3.setString(2, "5110");
            pstmt3.executeUpdate();

            // When: 試算表ビューを参照
            PreparedStatement selectStmt = conn.prepareStatement("""
                SELECT * FROM "試算表"
                WHERE account_code IN ('1020', '4010', '5110')
                ORDER BY account_code
                """);
            ResultSet rs = selectStmt.executeQuery();

            // Then: 各勘定科目の残高が正しく計算されている
            // 普通預金（借方残高）: 500,000 - 200,000 = 300,000
            assertThat(rs.next()).isTrue();
            assertThat(rs.getString("account_code")).isEqualTo("1020");
            assertThat(rs.getString("account_name")).isEqualTo("普通預金");
            assertThat(rs.getBigDecimal("debit_total")).isEqualByComparingTo(new BigDecimal("500000.00"));
            assertThat(rs.getBigDecimal("credit_total")).isEqualByComparingTo(new BigDecimal("200000.00"));
            assertThat(rs.getBigDecimal("balance")).isEqualByComparingTo(new BigDecimal("300000.00"));

            // 売上高（貸方残高）: 1,000,000 - 0 = 1,000,000
            assertThat(rs.next()).isTrue();
            assertThat(rs.getString("account_code")).isEqualTo("4010");
            assertThat(rs.getString("account_name")).isEqualTo("売上高");
            assertThat(rs.getBigDecimal("debit_total")).isEqualByComparingTo(new BigDecimal("0.00"));
            assertThat(rs.getBigDecimal("credit_total")).isEqualByComparingTo(new BigDecimal("1000000.00"));
            assertThat(rs.getBigDecimal("balance")).isEqualByComparingTo(new BigDecimal("1000000.00"));

            // 仕入（借方残高）: 600,000 - 0 = 600,000
            assertThat(rs.next()).isTrue();
            assertThat(rs.getString("account_code")).isEqualTo("5110");
            assertThat(rs.getString("account_name")).isEqualTo("仕入");
            assertThat(rs.getBigDecimal("debit_total")).isEqualByComparingTo(new BigDecimal("600000.00"));
            assertThat(rs.getBigDecimal("credit_total")).isEqualByComparingTo(new BigDecimal("0.00"));
            assertThat(rs.getBigDecimal("balance")).isEqualByComparingTo(new BigDecimal("600000.00"));
        }
    }

    @Test
    @Order(3)
    @DisplayName("決算仕訳フラグ=1のデータは除外される")
    void testExcludeSettlementJournals() throws SQLException {
        // Given: 通常仕訳と決算仕訳のデータを登録
        LocalDate date = LocalDate.of(2025, 3, 31);
        String accountCode = "5110";  // 仕入

        try (Connection conn = DriverManager.getConnection(
                POSTGRES.getJdbcUrl(),
                POSTGRES.getUsername(),
                POSTGRES.getPassword())) {

            // 通常仕訳（決算仕訳フラグ=0）
            PreparedStatement pstmt1 = conn.prepareStatement("""
                INSERT INTO "日次勘定科目残高" (
                    "起票日", "勘定科目コード", "補助科目コード", "部門コード",
                    "プロジェクトコード", "決算仕訳フラグ", "借方金額", "貸方金額"
                ) VALUES (?, ?, '', '', '', 0, 1000000.00, 0.00)
                """);
            pstmt1.setObject(1, date);
            pstmt1.setString(2, accountCode);
            pstmt1.executeUpdate();

            // 決算仕訳（決算仕訳フラグ=1）
            PreparedStatement pstmt2 = conn.prepareStatement("""
                INSERT INTO "日次勘定科目残高" (
                    "起票日", "勘定科目コード", "補助科目コード", "部門コード",
                    "プロジェクトコード", "決算仕訳フラグ", "借方金額", "貸方金額"
                ) VALUES (?, ?, '', '', '', 1, 50000.00, 0.00)
                """);
            pstmt2.setObject(1, date);
            pstmt2.setString(2, accountCode);
            pstmt2.executeUpdate();

            // When: 試算表ビューを参照
            PreparedStatement selectStmt = conn.prepareStatement("""
                SELECT * FROM "試算表"
                WHERE account_code = ?
                """);
            selectStmt.setString(1, accountCode);
            ResultSet rs = selectStmt.executeQuery();

            // Then: 通常仕訳のみが集計される
            assertThat(rs.next()).isTrue();
            assertThat(rs.getBigDecimal("debit_total")).isEqualByComparingTo(new BigDecimal("1000000.00"));
            assertThat(rs.getBigDecimal("balance")).isEqualByComparingTo(new BigDecimal("1000000.00"));
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
                ('4010', '売上高', '収益'::account_type, false, true, 0),
                ('5110', '仕入', '費用'::account_type, false, true, 0)
                ON CONFLICT ("勘定科目コード") DO NOTHING
                """);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }
}
