package com.example.accounting;

import com.example.accounting.testsetup.TestDatabase;
import org.junit.jupiter.api.*;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;

import java.math.BigDecimal;
import java.sql.*;

import static org.assertj.core.api.Assertions.*;

/**
 * 勘定科目マスタのリファクタリングテスト
 *
 * 実務で必要な項目（BSPL区分、取引要素区分、合計科目フラグ等）を段階的に追加します。
 */
@Testcontainers
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
class AccountRefactoringTest {

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
    @DisplayName("BSPL区分を設定できる")
    void testBsplDistinction() throws SQLException {
        // マイグレーション後、このテストが通るようになる
        String sql = """
            INSERT INTO "勘定科目マスタ"
            ("勘定科目コード", "勘定科目名", "勘定科目種別", "BSPL区分", "残高")
            VALUES (?, ?, ?::account_type, ?, ?)
            RETURNING "勘定科目コード", "BSPL区分"
            """;

        try (PreparedStatement pstmt = connection.prepareStatement(sql)) {
            pstmt.setString(1, "1000");
            pstmt.setString(2, "現金");
            pstmt.setString(3, "資産");
            pstmt.setString(4, "B");  // 貸借対照表
            pstmt.setBigDecimal(5, new BigDecimal("0"));

            ResultSet rs = pstmt.executeQuery();

            assertThat(rs.next()).isTrue();
            assertThat(rs.getString("BSPL区分")).isEqualTo("B");
        }
    }

    @Test
    @Order(2)
    @DisplayName("取引要素区分を設定できる")
    void testTransactionDistinction() throws SQLException {
        String sql = """
            INSERT INTO "勘定科目マスタ"
            ("勘定科目コード", "勘定科目名", "勘定科目種別", "BSPL区分", "取引要素区分", "残高")
            VALUES (?, ?, ?::account_type, ?, ?, ?)
            RETURNING "勘定科目コード", "取引要素区分"
            """;

        try (PreparedStatement pstmt = connection.prepareStatement(sql)) {
            pstmt.setString(1, "1000");
            pstmt.setString(2, "現金");
            pstmt.setString(3, "資産");
            pstmt.setString(4, "B");
            pstmt.setString(5, "1");  // 資産
            pstmt.setBigDecimal(6, new BigDecimal("0"));

            ResultSet rs = pstmt.executeQuery();

            assertThat(rs.next()).isTrue();
            assertThat(rs.getString("取引要素区分")).isEqualTo("1");
        }
    }

    @Test
    @Order(3)
    @DisplayName("集計科目を設定できる")
    void testSumAccount() throws SQLException {
        String sql = """
            INSERT INTO "勘定科目マスタ"
            ("勘定科目コード", "勘定科目名", "勘定科目種別", "合計科目", "残高")
            VALUES (?, ?, ?::account_type, ?, ?)
            RETURNING "勘定科目コード", "合計科目"
            """;

        try (PreparedStatement pstmt = connection.prepareStatement(sql)) {
            pstmt.setString(1, "1000");
            pstmt.setString(2, "流動資産");
            pstmt.setString(3, "資産");
            pstmt.setBoolean(4, true);  // 集計科目フラグ
            pstmt.setBigDecimal(5, new BigDecimal("0"));

            ResultSet rs = pstmt.executeQuery();

            assertThat(rs.next()).isTrue();
            assertThat(rs.getBoolean("合計科目")).isTrue();
        }
    }

    @Test
    @Order(4)
    @DisplayName("勘定科目カナを設定できる")
    void testAccountKana() throws SQLException {
        String sql = """
            INSERT INTO "勘定科目マスタ"
            ("勘定科目コード", "勘定科目名", "勘定科目種別", "勘定科目カナ", "残高")
            VALUES (?, ?, ?::account_type, ?, ?)
            RETURNING "勘定科目コード", "勘定科目カナ"
            """;

        try (PreparedStatement pstmt = connection.prepareStatement(sql)) {
            pstmt.setString(1, "1000");
            pstmt.setString(2, "現金");
            pstmt.setString(3, "資産");
            pstmt.setString(4, "ゲンキン");
            pstmt.setBigDecimal(5, new BigDecimal("0"));

            ResultSet rs = pstmt.executeQuery();

            assertThat(rs.next()).isTrue();
            assertThat(rs.getString("勘定科目カナ")).isEqualTo("ゲンキン");
        }
    }

    @Test
    @Order(5)
    @DisplayName("表示順序と集計対象を設定できる")
    void testDisplayOrderAndAggregation() throws SQLException {
        String sql = """
            INSERT INTO "勘定科目マスタ"
            ("勘定科目コード", "勘定科目名", "勘定科目種別", "表示順序", "集計対象", "残高")
            VALUES (?, ?, ?::account_type, ?, ?, ?)
            RETURNING "勘定科目コード", "表示順序", "集計対象"
            """;

        try (PreparedStatement pstmt = connection.prepareStatement(sql)) {
            pstmt.setString(1, "1000");
            pstmt.setString(2, "現金");
            pstmt.setString(3, "資産");
            pstmt.setInt(4, 100);
            pstmt.setBoolean(5, true);
            pstmt.setBigDecimal(6, new BigDecimal("0"));

            ResultSet rs = pstmt.executeQuery();

            assertThat(rs.next()).isTrue();
            assertThat(rs.getInt("表示順序")).isEqualTo(100);
            assertThat(rs.getBoolean("集計対象")).isTrue();
        }
    }
}
