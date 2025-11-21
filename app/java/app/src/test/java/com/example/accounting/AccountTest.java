package com.example.accounting;

import com.example.accounting.testsetup.TestDatabase;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;

import java.math.BigDecimal;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * 勘定科目マスタのテスト
 */
@Testcontainers
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
class AccountTest {

    @Container
    static PostgreSQLContainer<?> postgres = new PostgreSQLContainer<>("postgres:16-alpine")
            .withDatabaseName("testdb")
            .withUsername("testuser")
            .withPassword("testpass");

    private static TestDatabase testDb;
    private static Connection connection;

    @BeforeAll
    static void setUp() throws SQLException {
        testDb = new TestDatabase(postgres);
        testDb.start();
        connection = testDb.getConnection();
    }

    @AfterAll
    static void tearDown() throws SQLException {
        testDb.stop();
    }

    @BeforeEach
    void cleanup() throws SQLException {
        testDb.cleanup();
    }

    @Test
    @Order(1)
    @DisplayName("勘定科目を登録できる")
    void testCreateAccount() throws SQLException {
        // 1. テストデータを作成
        String sql = """
            INSERT INTO "勘定科目マスタ" ("勘定科目コード", "勘定科目名", "勘定科目種別", "残高")
            VALUES (?, ?, ?::account_type, ?)
            RETURNING "勘定科目ID", "勘定科目コード", "勘定科目名", "勘定科目種別", "残高"
            """;

        try (PreparedStatement pstmt = connection.prepareStatement(sql)) {
            pstmt.setString(1, "1000");
            pstmt.setString(2, "現金");
            pstmt.setString(3, "資産");
            pstmt.setBigDecimal(4, new BigDecimal("50000.00"));

            ResultSet rs = pstmt.executeQuery();

            // 2. 取得したデータが期待通りか検証
            assertThat(rs.next()).isTrue();
            assertThat(rs.getString("勘定科目コード")).isEqualTo("1000");
            assertThat(rs.getString("勘定科目名")).isEqualTo("現金");
            assertThat(rs.getString("勘定科目種別")).isEqualTo("資産");
            assertThat(rs.getBigDecimal("残高")).isEqualByComparingTo(new BigDecimal("50000.00"));
        }
    }

    @Test
    @Order(2)
    @DisplayName("すべての勘定科目を取得できる")
    void testFindAllAccounts() throws SQLException {
        // 1. 複数の勘定科目を登録
        insertAccount("1000", "現金", "資産", "50000.00");
        insertAccount("2000", "買掛金", "負債", "30000.00");
        insertAccount("3000", "資本金", "純資産", "100000.00");

        // 2. すべての勘定科目を取得
        String sql = """
            SELECT "勘定科目コード", "勘定科目名", "勘定科目種別", "残高"
            FROM "勘定科目マスタ"
            ORDER BY "勘定科目コード"
            """;

        List<String> codes = new ArrayList<>();
        try (Statement stmt = connection.createStatement();
             ResultSet rs = stmt.executeQuery(sql)) {
            while (rs.next()) {
                codes.add(rs.getString("勘定科目コード"));
            }
        }

        // 3. 期待通りのデータが取得できるか検証
        assertThat(codes).hasSize(3);
        assertThat(codes).containsExactly("1000", "2000", "3000");
    }

    @Test
    @Order(3)
    @DisplayName("勘定科目コードで検索できる")
    void testFindAccountByCode() throws SQLException {
        // 1. テストデータを登録
        insertAccount("1000", "現金", "資産", "50000.00");

        // 2. コードで検索
        String sql = """
            SELECT "勘定科目コード", "勘定科目名", "勘定科目種別"
            FROM "勘定科目マスタ"
            WHERE "勘定科目コード" = ?
            """;

        try (PreparedStatement pstmt = connection.prepareStatement(sql)) {
            pstmt.setString(1, "1000");
            ResultSet rs = pstmt.executeQuery();

            // 3. 正しいデータが取得できるか検証
            assertThat(rs.next()).isTrue();
            assertThat(rs.getString("勘定科目名")).isEqualTo("現金");
            assertThat(rs.getString("勘定科目種別")).isEqualTo("資産");
        }
    }

    @Test
    @Order(4)
    @DisplayName("勘定科目を更新できる")
    void testUpdateAccount() throws SQLException {
        // 1. データを登録
        int accountId = insertAccount("1000", "現金", "資産", "50000.00");

        // 2. データを更新
        String updateSql = """
            UPDATE "勘定科目マスタ"
            SET "勘定科目名" = ?, "残高" = ?, "更新日時" = CURRENT_TIMESTAMP
            WHERE "勘定科目ID" = ?
            """;

        try (PreparedStatement pstmt = connection.prepareStatement(updateSql)) {
            pstmt.setString(1, "現金及び預金");
            pstmt.setBigDecimal(2, new BigDecimal("75000.00"));
            pstmt.setInt(3, accountId);
            int updated = pstmt.executeUpdate();
            assertThat(updated).isEqualTo(1);
        }

        // 3. 更新されたか検証
        String selectSql = """
            SELECT "勘定科目コード", "勘定科目名", "残高"
            FROM "勘定科目マスタ"
            WHERE "勘定科目ID" = ?
            """;

        try (PreparedStatement pstmt = connection.prepareStatement(selectSql)) {
            pstmt.setInt(1, accountId);
            ResultSet rs = pstmt.executeQuery();

            assertThat(rs.next()).isTrue();
            assertThat(rs.getString("勘定科目名")).isEqualTo("現金及び預金");
            assertThat(rs.getBigDecimal("残高")).isEqualByComparingTo(new BigDecimal("75000.00"));
            assertThat(rs.getString("勘定科目コード")).isEqualTo("1000"); // 変更していない項目は保持される
        }
    }

    @Test
    @Order(5)
    @DisplayName("勘定科目を削除できる")
    void testDeleteAccount() throws SQLException {
        // 1. データを登録
        int accountId = insertAccount("1000", "現金", "資産", "50000.00");

        // 2. データを削除
        String deleteSql = """
            DELETE FROM "勘定科目マスタ"
            WHERE "勘定科目ID" = ?
            """;

        try (PreparedStatement pstmt = connection.prepareStatement(deleteSql)) {
            pstmt.setInt(1, accountId);
            int deleted = pstmt.executeUpdate();
            assertThat(deleted).isEqualTo(1);
        }

        // 3. データが削除されたか検証
        String selectSql = """
            SELECT COUNT(*) as count
            FROM "勘定科目マスタ"
            WHERE "勘定科目ID" = ?
            """;

        try (PreparedStatement pstmt = connection.prepareStatement(selectSql)) {
            pstmt.setInt(1, accountId);
            ResultSet rs = pstmt.executeQuery();

            assertThat(rs.next()).isTrue();
            assertThat(rs.getInt("count")).isEqualTo(0);
        }
    }

    @Test
    @Order(6)
    @DisplayName("勘定科目種別でフィルタリングできる")
    void testFilterAccountsByType() throws SQLException {
        // 1. 複数の勘定科目を登録
        insertAccount("1000", "現金", "資産", "50000.00");
        insertAccount("2000", "買掛金", "負債", "30000.00");
        insertAccount("3000", "資本金", "純資産", "100000.00");

        // 2. 資産勘定のみを取得
        String sql = """
            SELECT "勘定科目名"
            FROM "勘定科目マスタ"
            WHERE "勘定科目種別" = ?::account_type
            """;

        List<String> assetNames = new ArrayList<>();
        try (PreparedStatement pstmt = connection.prepareStatement(sql)) {
            pstmt.setString(1, "資産");
            ResultSet rs = pstmt.executeQuery();
            while (rs.next()) {
                assetNames.add(rs.getString("勘定科目名"));
            }
        }

        // 3. 正しくフィルタリングされるか検証
        assertThat(assetNames).hasSize(1);
        assertThat(assetNames).containsExactly("現金");

        // 4. 負債勘定のみを取得
        List<String> liabilityNames = new ArrayList<>();
        try (PreparedStatement pstmt = connection.prepareStatement(sql)) {
            pstmt.setString(1, "負債");
            ResultSet rs = pstmt.executeQuery();
            while (rs.next()) {
                liabilityNames.add(rs.getString("勘定科目名"));
            }
        }

        assertThat(liabilityNames).hasSize(1);
        assertThat(liabilityNames).containsExactly("買掛金");
    }

    /**
     * 勘定科目を登録するヘルパーメソッド
     */
    private int insertAccount(String code, String name, String type, String balance) throws SQLException {
        String sql = """
            INSERT INTO "勘定科目マスタ" ("勘定科目コード", "勘定科目名", "勘定科目種別", "残高")
            VALUES (?, ?, ?::account_type, ?)
            RETURNING "勘定科目ID"
            """;

        try (PreparedStatement pstmt = connection.prepareStatement(sql)) {
            pstmt.setString(1, code);
            pstmt.setString(2, name);
            pstmt.setString(3, type);
            pstmt.setBigDecimal(4, new BigDecimal(balance));

            ResultSet rs = pstmt.executeQuery();
            if (rs.next()) {
                return rs.getInt("勘定科目ID");
            }
            throw new SQLException("勘定科目の登録に失敗しました");
        }
    }
}
