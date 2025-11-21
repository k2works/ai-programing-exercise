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
 * 残高管理サービスのテスト
 */
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
@DisplayName("残高管理サービス - UPSERT テスト")
class BalanceServiceTest extends TestDatabaseConfig {

    private BalanceService balanceService;

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
    @DisplayName("新規の日次残高を登録できる")
    void testInsertNewDailyBalance() throws SQLException {
        // Given: BalanceService のインスタンス
        balanceService = new BalanceService(
                postgres.getJdbcUrl(),
                postgres.getUsername(),
                postgres.getPassword()
        );

        LocalDate entryDate = LocalDate.of(2025, 1, 15);
        String accountCode = "1020";  // 普通預金
        BigDecimal debitAmount = new BigDecimal("100000.00");
        BigDecimal creditAmount = new BigDecimal("0.00");

        // When: 日次残高を更新（初回登録）
        balanceService.updateDailyBalance(
                entryDate,
                accountCode,
                "",
                "",
                "",
                0,
                debitAmount,
                creditAmount
        );

        // Then: データが正しく登録されている
        try (Connection conn = DriverManager.getConnection(
                postgres.getJdbcUrl(),
                postgres.getUsername(),
                postgres.getPassword())) {

            PreparedStatement selectStmt = conn.prepareStatement("""
                SELECT * FROM "日次勘定科目残高"
                WHERE "起票日" = ? AND "勘定科目コード" = ?
                """);
            selectStmt.setObject(1, entryDate);
            selectStmt.setString(2, accountCode);
            ResultSet rs = selectStmt.executeQuery();

            assertThat(rs.next()).isTrue();
            assertThat(rs.getBigDecimal("借方金額")).isEqualByComparingTo(debitAmount);
            assertThat(rs.getBigDecimal("貸方金額")).isEqualByComparingTo(creditAmount);
        }
    }

    @Test
    @Order(2)
    @DisplayName("既存の日次残高を加算更新できる（UPSERT）")
    void testUpsertDailyBalance() throws SQLException {
        // Given: 既存の日次残高が存在
        balanceService = new BalanceService(
                postgres.getJdbcUrl(),
                postgres.getUsername(),
                postgres.getPassword()
        );

        LocalDate entryDate = LocalDate.of(2025, 1, 15);
        String accountCode = "1020";  // 普通預金

        // 初回登録
        balanceService.updateDailyBalance(
                entryDate,
                accountCode,
                "",
                "",
                "",
                0,
                new BigDecimal("100000.00"),
                new BigDecimal("0.00")
        );

        // When: 同じキーで2回目の更新（加算）
        balanceService.updateDailyBalance(
                entryDate,
                accountCode,
                "",
                "",
                "",
                0,
                new BigDecimal("50000.00"),
                new BigDecimal("30000.00")
        );

        // Then: 金額が加算されている
        try (Connection conn = DriverManager.getConnection(
                postgres.getJdbcUrl(),
                postgres.getUsername(),
                postgres.getPassword())) {

            PreparedStatement selectStmt = conn.prepareStatement("""
                SELECT * FROM "日次勘定科目残高"
                WHERE "起票日" = ? AND "勘定科目コード" = ?
                """);
            selectStmt.setObject(1, entryDate);
            selectStmt.setString(2, accountCode);
            ResultSet rs = selectStmt.executeQuery();

            assertThat(rs.next()).isTrue();
            // 100000 + 50000 = 150000
            assertThat(rs.getBigDecimal("借方金額")).isEqualByComparingTo(new BigDecimal("150000.00"));
            // 0 + 30000 = 30000
            assertThat(rs.getBigDecimal("貸方金額")).isEqualByComparingTo(new BigDecimal("30000.00"));
        }
    }

    @Test
    @Order(3)
    @DisplayName("複数回の更新で正しく累積される")
    void testMultipleUpserts() throws SQLException {
        // Given: BalanceService のインスタンス
        balanceService = new BalanceService(
                postgres.getJdbcUrl(),
                postgres.getUsername(),
                postgres.getPassword()
        );

        LocalDate entryDate = LocalDate.of(2025, 1, 15);
        String accountCode = "1020";

        // When: 複数回更新
        balanceService.updateDailyBalance(entryDate, accountCode, "", "", "", 0,
                new BigDecimal("10000.00"), new BigDecimal("0.00"));

        balanceService.updateDailyBalance(entryDate, accountCode, "", "", "", 0,
                new BigDecimal("20000.00"), new BigDecimal("5000.00"));

        balanceService.updateDailyBalance(entryDate, accountCode, "", "", "", 0,
                new BigDecimal("30000.00"), new BigDecimal("10000.00"));

        // Then: 全ての金額が累積されている
        try (Connection conn = DriverManager.getConnection(
                postgres.getJdbcUrl(),
                postgres.getUsername(),
                postgres.getPassword())) {

            PreparedStatement selectStmt = conn.prepareStatement("""
                SELECT * FROM "日次勘定科目残高"
                WHERE "起票日" = ? AND "勘定科目コード" = ?
                """);
            selectStmt.setObject(1, entryDate);
            selectStmt.setString(2, accountCode);
            ResultSet rs = selectStmt.executeQuery();

            assertThat(rs.next()).isTrue();
            // 10000 + 20000 + 30000 = 60000
            assertThat(rs.getBigDecimal("借方金額")).isEqualByComparingTo(new BigDecimal("60000.00"));
            // 0 + 5000 + 10000 = 15000
            assertThat(rs.getBigDecimal("貸方金額")).isEqualByComparingTo(new BigDecimal("15000.00"));
        }
    }

    @Test
    @Order(4)
    @DisplayName("部門コードが異なる場合は別レコードとして管理される")
    void testDifferentDepartmentCodes() throws SQLException {
        // Given: BalanceService のインスタンス
        balanceService = new BalanceService(
                postgres.getJdbcUrl(),
                postgres.getUsername(),
                postgres.getPassword()
        );

        LocalDate entryDate = LocalDate.of(2025, 1, 15);
        String accountCode = "4010";  // 売上高

        // When: 部門001と部門002の残高を更新
        balanceService.updateDailyBalance(entryDate, accountCode, "", "001", "", 0,
                new BigDecimal("0.00"), new BigDecimal("300000.00"));

        balanceService.updateDailyBalance(entryDate, accountCode, "", "002", "", 0,
                new BigDecimal("0.00"), new BigDecimal("200000.00"));

        // Then: 2つの別レコードとして登録されている
        try (Connection conn = DriverManager.getConnection(
                postgres.getJdbcUrl(),
                postgres.getUsername(),
                postgres.getPassword())) {

            PreparedStatement selectStmt = conn.prepareStatement("""
                SELECT * FROM "日次勘定科目残高"
                WHERE "起票日" = ? AND "勘定科目コード" = ?
                ORDER BY "部門コード"
                """);
            selectStmt.setObject(1, entryDate);
            selectStmt.setString(2, accountCode);
            ResultSet rs = selectStmt.executeQuery();

            assertThat(rs.next()).isTrue();
            assertThat(rs.getString("部門コード")).isEqualTo("001");
            assertThat(rs.getBigDecimal("貸方金額")).isEqualByComparingTo(new BigDecimal("300000.00"));

            assertThat(rs.next()).isTrue();
            assertThat(rs.getString("部門コード")).isEqualTo("002");
            assertThat(rs.getBigDecimal("貸方金額")).isEqualByComparingTo(new BigDecimal("200000.00"));
        }
    }

    @Test
    @Order(5)
    @DisplayName("補助科目コードが異なる場合は別レコードとして管理される")
    void testDifferentSubAccountCodes() throws SQLException {
        // Given: BalanceService のインスタンス
        balanceService = new BalanceService(
                postgres.getJdbcUrl(),
                postgres.getUsername(),
                postgres.getPassword()
        );

        LocalDate entryDate = LocalDate.of(2025, 1, 15);
        String accountCode = "1130";  // 売掛金

        // When: 補助科目A001とA002の残高を更新
        balanceService.updateDailyBalance(entryDate, accountCode, "A001", "", "", 0,
                new BigDecimal("500000.00"), new BigDecimal("0.00"));

        balanceService.updateDailyBalance(entryDate, accountCode, "A002", "", "", 0,
                new BigDecimal("300000.00"), new BigDecimal("0.00"));

        // Then: 2つの別レコードとして登録されている
        try (Connection conn = DriverManager.getConnection(
                postgres.getJdbcUrl(),
                postgres.getUsername(),
                postgres.getPassword())) {

            PreparedStatement selectStmt = conn.prepareStatement("""
                SELECT * FROM "日次勘定科目残高"
                WHERE "起票日" = ? AND "勘定科目コード" = ?
                ORDER BY "補助科目コード"
                """);
            selectStmt.setObject(1, entryDate);
            selectStmt.setString(2, accountCode);
            ResultSet rs = selectStmt.executeQuery();

            assertThat(rs.next()).isTrue();
            assertThat(rs.getString("補助科目コード")).isEqualTo("A001");
            assertThat(rs.getBigDecimal("借方金額")).isEqualByComparingTo(new BigDecimal("500000.00"));

            assertThat(rs.next()).isTrue();
            assertThat(rs.getString("補助科目コード")).isEqualTo("A002");
            assertThat(rs.getBigDecimal("借方金額")).isEqualByComparingTo(new BigDecimal("300000.00"));
        }
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
                INSERT INTO "勘定科目マスタ" ("勘定科目コード", "勘定科目名", "勘定科目種別", "合計科目", "集計対象", "残高")
                VALUES
                ('1020', '普通預金', '資産'::account_type, false, true, 0),
                ('1130', '売掛金', '資産'::account_type, false, true, 0),
                ('4010', '売上高', '収益'::account_type, false, true, 0)
                ON CONFLICT ("勘定科目コード") DO NOTHING
                """);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }
}
