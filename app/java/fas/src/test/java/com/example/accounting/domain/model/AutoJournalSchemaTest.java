package com.example.accounting.domain.model;

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

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.sql.Timestamp;
import java.time.LocalDateTime;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * 自動仕訳スキーマのテスト
 *
 * 自動仕訳管理、パターン、パターン明細、実行ログの4つのテーブルを検証
 */
@Testcontainers
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
class AutoJournalSchemaTest {

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
        try (Statement stmt = connection.createStatement()) {
            stmt.execute("TRUNCATE TABLE \"自動仕訳実行ログ\" CASCADE");
            stmt.execute("TRUNCATE TABLE \"自動仕訳パターン明細\" CASCADE");
            stmt.execute("TRUNCATE TABLE \"自動仕訳パターン\" CASCADE");
            stmt.execute("TRUNCATE TABLE \"自動仕訳管理\" CASCADE");
            stmt.execute("TRUNCATE TABLE \"勘定科目マスタ\" CASCADE");
        }

        insertTestAccounts();
    }

    @Test
    @Order(1)
    @DisplayName("自動仕訳管理テーブルにソーステーブル名を登録できる")
    void testInsertAutoJournalManagement() throws SQLException {
        // Given: 自動仕訳管理レコードを挿入
        String sql = """
            INSERT INTO "自動仕訳管理" ("ソーステーブル名", "最終処理日時")
            VALUES (?, ?)
            """;

        LocalDateTime lastProcessed = LocalDateTime.of(2025, 1, 1, 0, 0, 0);

        try (PreparedStatement pstmt = connection.prepareStatement(sql)) {
            pstmt.setString(1, "売上テーブル");
            pstmt.setTimestamp(2, Timestamp.valueOf(lastProcessed));
            pstmt.executeUpdate();
        }

        // When: データを取得
        String selectSql = "SELECT * FROM \"自動仕訳管理\" WHERE \"ソーステーブル名\" = ?";
        try (PreparedStatement pstmt = connection.prepareStatement(selectSql)) {
            pstmt.setString(1, "売上テーブル");
            ResultSet rs = pstmt.executeQuery();

            // Then: 正しく登録されている
            assertThat(rs.next()).isTrue();
            assertThat(rs.getString("ソーステーブル名")).isEqualTo("売上テーブル");
            assertThat(rs.getTimestamp("最終処理日時").toLocalDateTime()).isEqualTo(lastProcessed);
        }
    }

    @Test
    @Order(2)
    @DisplayName("自動仕訳パターンと明細を登録できる")
    void testInsertAutoJournalPattern() throws SQLException {
        // Given: パターンを挿入
        String patternSql = """
            INSERT INTO "自動仕訳パターン" (
                "パターンコード", "パターン名", "ソーステーブル名", "説明", "有効フラグ"
            ) VALUES (?, ?, ?, ?, ?)
            RETURNING "ID"
            """;

        long patternId;
        try (PreparedStatement pstmt = connection.prepareStatement(patternSql)) {
            pstmt.setString(1, "SALES_001");
            pstmt.setString(2, "売上仕訳自動生成");
            pstmt.setString(3, "売上テーブル");
            pstmt.setString(4, "売上データから仕訳を自動生成");
            pstmt.setBoolean(5, true);

            ResultSet rs = pstmt.executeQuery();
            rs.next();
            patternId = rs.getLong("ID");
        }

        // パターン明細を挿入（借方：売掛金、貸方：売上）
        String itemSql = """
            INSERT INTO "自動仕訳パターン明細" (
                "パターンID", "行番号", "貸借区分", "勘定科目コード", "金額式", "摘要テンプレート"
            ) VALUES (?, ?, ?, ?, ?, ?)
            """;

        // 借方
        try (PreparedStatement pstmt = connection.prepareStatement(itemSql)) {
            pstmt.setLong(1, patternId);
            pstmt.setInt(2, 1);
            pstmt.setString(3, "D");
            pstmt.setString(4, "1300");
            pstmt.setString(5, "amount");
            pstmt.setString(6, "売掛金計上");
            pstmt.executeUpdate();
        }

        // 貸方
        try (PreparedStatement pstmt = connection.prepareStatement(itemSql)) {
            pstmt.setLong(1, patternId);
            pstmt.setInt(2, 2);
            pstmt.setString(3, "C");
            pstmt.setString(4, "4100");
            pstmt.setString(5, "amount");
            pstmt.setString(6, "売上計上");
            pstmt.executeUpdate();
        }

        // When: パターンと明細を取得
        String selectPatternSql = """
            SELECT p.*, COUNT(i."ID") as item_count
            FROM "自動仕訳パターン" p
            LEFT JOIN "自動仕訳パターン明細" i ON p."ID" = i."パターンID"
            WHERE p."パターンコード" = ?
            GROUP BY p."ID"
            """;

        try (PreparedStatement pstmt = connection.prepareStatement(selectPatternSql)) {
            pstmt.setString(1, "SALES_001");
            ResultSet rs = pstmt.executeQuery();

            // Then: パターンと明細が正しく登録されている
            assertThat(rs.next()).isTrue();
            assertThat(rs.getString("パターン名")).isEqualTo("売上仕訳自動生成");
            assertThat(rs.getBoolean("有効フラグ")).isTrue();
            assertThat(rs.getInt("item_count")).isEqualTo(2);
        }
    }

    @Test
    @Order(3)
    @DisplayName("自動仕訳実行ログを登録できる")
    void testInsertAutoJournalLog() throws SQLException {
        // Given: パターンを挿入
        String patternSql = """
            INSERT INTO "自動仕訳パターン" (
                "パターンコード", "パターン名", "ソーステーブル名", "有効フラグ"
            ) VALUES (?, ?, ?, ?)
            RETURNING "ID"
            """;

        long patternId;
        try (PreparedStatement pstmt = connection.prepareStatement(patternSql)) {
            pstmt.setString(1, "TEST_001");
            pstmt.setString(2, "テストパターン");
            pstmt.setString(3, "テストテーブル");
            pstmt.setBoolean(4, true);

            ResultSet rs = pstmt.executeQuery();
            rs.next();
            patternId = rs.getLong("ID");
        }

        // 実行ログを挿入
        String logSql = """
            INSERT INTO "自動仕訳実行ログ" (
                "パターンID", "実行日時", "処理件数", "生成件数", "ステータス", "メッセージ"
            ) VALUES (?, ?, ?, ?, ?, ?)
            """;

        LocalDateTime executedAt = LocalDateTime.of(2025, 1, 1, 10, 0, 0);

        try (PreparedStatement pstmt = connection.prepareStatement(logSql)) {
            pstmt.setLong(1, patternId);
            pstmt.setTimestamp(2, Timestamp.valueOf(executedAt));
            pstmt.setInt(3, 100);
            pstmt.setInt(4, 50);
            pstmt.setString(5, "SUCCESS");
            pstmt.setString(6, "正常終了");
            pstmt.executeUpdate();
        }

        // When: ログを取得
        String selectSql = """
            SELECT * FROM "自動仕訳実行ログ"
            WHERE "パターンID" = ?
            """;

        try (PreparedStatement pstmt = connection.prepareStatement(selectSql)) {
            pstmt.setLong(1, patternId);
            ResultSet rs = pstmt.executeQuery();

            // Then: ログが正しく登録されている
            assertThat(rs.next()).isTrue();
            assertThat(rs.getInt("処理件数")).isEqualTo(100);
            assertThat(rs.getInt("生成件数")).isEqualTo(50);
            assertThat(rs.getString("ステータス")).isEqualTo("SUCCESS");
            assertThat(rs.getString("メッセージ")).isEqualTo("正常終了");
        }
    }

    @Test
    @Order(4)
    @DisplayName("パターン削除時に明細も削除される（CASCADE）")
    void testCascadeDeletePattern() throws SQLException {
        // Given: パターンと明細を挿入
        String patternSql = """
            INSERT INTO "自動仕訳パターン" (
                "パターンコード", "パターン名", "ソーステーブル名", "有効フラグ"
            ) VALUES (?, ?, ?, ?)
            RETURNING "ID"
            """;

        long patternId;
        try (PreparedStatement pstmt = connection.prepareStatement(patternSql)) {
            pstmt.setString(1, "DELETE_TEST");
            pstmt.setString(2, "削除テスト");
            pstmt.setString(3, "テストテーブル");
            pstmt.setBoolean(4, true);

            ResultSet rs = pstmt.executeQuery();
            rs.next();
            patternId = rs.getLong("ID");
        }

        String itemSql = """
            INSERT INTO "自動仕訳パターン明細" (
                "パターンID", "行番号", "貸借区分", "勘定科目コード", "金額式"
            ) VALUES (?, ?, ?, ?, ?)
            """;

        try (PreparedStatement pstmt = connection.prepareStatement(itemSql)) {
            pstmt.setLong(1, patternId);
            pstmt.setInt(2, 1);
            pstmt.setString(3, "D");
            pstmt.setString(4, "1100");
            pstmt.setString(5, "amount");
            pstmt.executeUpdate();
        }

        // When: パターンを削除
        String deleteSql = "DELETE FROM \"自動仕訳パターン\" WHERE \"ID\" = ?";
        try (PreparedStatement pstmt = connection.prepareStatement(deleteSql)) {
            pstmt.setLong(1, patternId);
            pstmt.executeUpdate();
        }

        // Then: 明細も削除されている
        String selectSql = "SELECT COUNT(*) FROM \"自動仕訳パターン明細\" WHERE \"パターンID\" = ?";
        try (PreparedStatement pstmt = connection.prepareStatement(selectSql)) {
            pstmt.setLong(1, patternId);
            ResultSet rs = pstmt.executeQuery();
            rs.next();
            assertThat(rs.getInt(1)).isEqualTo(0);
        }
    }

    @Test
    @Order(5)
    @DisplayName("CHECK制約：貸借区分はDまたはCのみ許可")
    void testCheckConstraintDebitCreditFlag() throws SQLException {
        // Given: パターンを挿入
        String patternSql = """
            INSERT INTO "自動仕訳パターン" (
                "パターンコード", "パターン名", "ソーステーブル名", "有効フラグ"
            ) VALUES (?, ?, ?, ?)
            RETURNING "ID"
            """;

        long patternId;
        try (PreparedStatement pstmt = connection.prepareStatement(patternSql)) {
            pstmt.setString(1, "CHECK_TEST");
            pstmt.setString(2, "制約テスト");
            pstmt.setString(3, "テストテーブル");
            pstmt.setBoolean(4, true);

            ResultSet rs = pstmt.executeQuery();
            rs.next();
            patternId = rs.getLong("ID");
        }

        // When/Then: 無効な貸借区分を挿入しようとするとエラー
        String itemSql = """
            INSERT INTO "自動仕訳パターン明細" (
                "パターンID", "行番号", "貸借区分", "勘定科目コード", "金額式"
            ) VALUES (?, ?, ?, ?, ?)
            """;

        try (PreparedStatement pstmt = connection.prepareStatement(itemSql)) {
            pstmt.setLong(1, patternId);
            pstmt.setInt(2, 1);
            pstmt.setString(3, "X"); // 無効な値
            pstmt.setString(4, "1100");
            pstmt.setString(5, "amount");

            boolean exceptionThrown = false;
            try {
                pstmt.executeUpdate();
            } catch (SQLException e) {
                exceptionThrown = true;
                assertThat(e.getMessage()).contains("check_貸借区分");
            }

            assertThat(exceptionThrown).isTrue();
        }
    }

    @Test
    @Order(6)
    @DisplayName("CHECK制約：ステータスはSUCCESS、FAILURE、RUNNINGのみ許可")
    void testCheckConstraintStatus() throws SQLException {
        // Given: パターンを挿入
        String patternSql = """
            INSERT INTO "自動仕訳パターン" (
                "パターンコード", "パターン名", "ソーステーブル名", "有効フラグ"
            ) VALUES (?, ?, ?, ?)
            RETURNING "ID"
            """;

        long patternId;
        try (PreparedStatement pstmt = connection.prepareStatement(patternSql)) {
            pstmt.setString(1, "STATUS_TEST");
            pstmt.setString(2, "ステータステスト");
            pstmt.setString(3, "テストテーブル");
            pstmt.setBoolean(4, true);

            ResultSet rs = pstmt.executeQuery();
            rs.next();
            patternId = rs.getLong("ID");
        }

        // When/Then: 無効なステータスを挿入しようとするとエラー
        String logSql = """
            INSERT INTO "自動仕訳実行ログ" (
                "パターンID", "実行日時", "ステータス"
            ) VALUES (?, ?, ?)
            """;

        try (PreparedStatement pstmt = connection.prepareStatement(logSql)) {
            pstmt.setLong(1, patternId);
            pstmt.setTimestamp(2, Timestamp.valueOf(LocalDateTime.now()));
            pstmt.setString(3, "INVALID"); // 無効な値

            boolean exceptionThrown = false;
            try {
                pstmt.executeUpdate();
            } catch (SQLException e) {
                exceptionThrown = true;
                assertThat(e.getMessage()).contains("check_ステータス");
            }

            assertThat(exceptionThrown).isTrue();
        }
    }

    /**
     * テスト用勘定科目を登録
     */
    private void insertTestAccounts() throws SQLException {
        String sql = """
            INSERT INTO "勘定科目マスタ" ("勘定科目コード", "勘定科目名", "勘定科目種別", "合計科目", "集計対象", "残高")
            VALUES
            ('1100', '現金', '資産'::account_type, false, true, 0),
            ('1300', '売掛金', '資産'::account_type, false, true, 0),
            ('4100', '売上', '収益'::account_type, false, true, 0)
            """;

        try (Statement stmt = connection.createStatement()) {
            stmt.executeUpdate(sql);
        }
    }
}
