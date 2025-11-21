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
import java.sql.Date;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.time.LocalDate;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * 3層構造仕訳テーブルのテスト
 *
 * 3層構造: 仕訳（ヘッダー） + 仕訳明細（明細行） + 仕訳貸借明細（借方・貸方）
 */
@Testcontainers
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
class Journal3LayerTest {

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
        // 各テスト前にデータをクリア（CASCADE で自動削除される）
        try (Statement stmt = connection.createStatement()) {
            stmt.execute("TRUNCATE TABLE \"仕訳\" CASCADE");
            stmt.execute("TRUNCATE TABLE \"勘定科目マスタ\" CASCADE");
        }

        // テスト用勘定科目を登録
        insertTestAccounts();
    }

    @Test
    @Order(1)
    @DisplayName("3層構造での仕訳登録_単純な仕訳")
    void test_3層構造での仕訳登録_単純な仕訳() throws SQLException {
        // Given: 現金100,000円で商品を仕入れる仕訳
        String journalNo = "JE-20250101-001";
        LocalDate journalDate = LocalDate.of(2025, 1, 1);
        LocalDate inputDate = LocalDate.of(2025, 1, 1);

        // When: 仕訳を登録
        // 1. 仕訳ヘッダー
        String journalSql = """
            INSERT INTO "仕訳" (
                "仕訳伝票番号", "起票日", "入力日", "決算仕訳フラグ", "単振フラグ",
                "仕訳伝票区分", "定期計上フラグ", "赤伝フラグ"
            ) VALUES (?, ?, ?, 0, 1, 0, 0, 0)
            """;
        try (PreparedStatement pstmt = connection.prepareStatement(journalSql)) {
            pstmt.setString(1, journalNo);
            pstmt.setDate(2, Date.valueOf(journalDate));
            pstmt.setDate(3, Date.valueOf(inputDate));
            pstmt.executeUpdate();
        }

        // 2. 仕訳明細（1行）
        String detailSql = """
            INSERT INTO "仕訳明細" (
                "仕訳伝票番号", "仕訳行番号", "行摘要"
            ) VALUES (?, 1, '商品仕入')
            """;
        try (PreparedStatement pstmt = connection.prepareStatement(detailSql)) {
            pstmt.setString(1, journalNo);
            pstmt.executeUpdate();
        }

        // 3. 仕訳貸借明細（借方：仕入、貸方：現金）
        String itemSql = """
            INSERT INTO "仕訳貸借明細" (
                "仕訳伝票番号", "仕訳行番号", "仕訳行貸借区分",
                "通貨コード", "為替レート", "勘定科目コード",
                "仕訳金額", "基軸換算仕訳金額", "資金繰フラグ"
            ) VALUES (?, 1, ?, 'JPY', 1.00, ?, ?, ?, ?)
            """;

        // 借方：仕入 100,000
        try (PreparedStatement pstmt = connection.prepareStatement(itemSql)) {
            pstmt.setString(1, journalNo);
            pstmt.setString(2, "D");
            pstmt.setString(3, "5110");
            pstmt.setBigDecimal(4, new BigDecimal("100000.00"));
            pstmt.setBigDecimal(5, new BigDecimal("100000.00"));
            pstmt.setInt(6, 0);
            pstmt.executeUpdate();
        }

        // 貸方：現金 100,000
        try (PreparedStatement pstmt = connection.prepareStatement(itemSql)) {
            pstmt.setString(1, journalNo);
            pstmt.setString(2, "C");
            pstmt.setString(3, "1100");
            pstmt.setBigDecimal(4, new BigDecimal("100000.00"));
            pstmt.setBigDecimal(5, new BigDecimal("100000.00"));
            pstmt.setInt(6, 0);
            pstmt.executeUpdate();
        }

        // Then: データが正しく登録されていることを確認
        // 1. 仕訳が登録されている
        String journalCountSql = "SELECT COUNT(*) as count FROM \"仕訳\" WHERE \"仕訳伝票番号\" = ?";
        try (PreparedStatement pstmt = connection.prepareStatement(journalCountSql)) {
            pstmt.setString(1, journalNo);
            ResultSet rs = pstmt.executeQuery();
            rs.next();
            assertThat(rs.getInt("count")).isEqualTo(1);
        }

        // 2. 仕訳明細が登録されている
        String detailCountSql = "SELECT COUNT(*) as count FROM \"仕訳明細\" WHERE \"仕訳伝票番号\" = ?";
        try (PreparedStatement pstmt = connection.prepareStatement(detailCountSql)) {
            pstmt.setString(1, journalNo);
            ResultSet rs = pstmt.executeQuery();
            rs.next();
            assertThat(rs.getInt("count")).isEqualTo(1);
        }

        // 3. 仕訳貸借明細が2件（借方・貸方）登録されている
        String itemCountSql = "SELECT COUNT(*) as count FROM \"仕訳貸借明細\" WHERE \"仕訳伝票番号\" = ?";
        try (PreparedStatement pstmt = connection.prepareStatement(itemCountSql)) {
            pstmt.setString(1, journalNo);
            ResultSet rs = pstmt.executeQuery();
            rs.next();
            assertThat(rs.getInt("count")).isEqualTo(2);
        }

        // 4. 借方・貸方の合計が一致する（複式簿記の原理）
        String balanceSql = """
            SELECT
                SUM(CASE WHEN "仕訳行貸借区分" = 'D' THEN "仕訳金額" ELSE 0 END) as debit_total,
                SUM(CASE WHEN "仕訳行貸借区分" = 'C' THEN "仕訳金額" ELSE 0 END) as credit_total
            FROM "仕訳貸借明細"
            WHERE "仕訳伝票番号" = ?
            """;
        try (PreparedStatement pstmt = connection.prepareStatement(balanceSql)) {
            pstmt.setString(1, journalNo);
            ResultSet rs = pstmt.executeQuery();
            rs.next();

            BigDecimal debitTotal = rs.getBigDecimal("debit_total");
            BigDecimal creditTotal = rs.getBigDecimal("credit_total");

            assertThat(debitTotal).isEqualByComparingTo(creditTotal);
            assertThat(debitTotal).isEqualByComparingTo(new BigDecimal("100000.00"));
        }
    }

    @Test
    @Order(2)
    @DisplayName("3層構造での仕訳登録_複合仕訳")
    void test_3層構造での仕訳登録_複合仕訳() throws SQLException {
        // Given: 売掛金の回収（振込手数料差引）
        // 売掛金 100,000円 → 普通預金 99,560円 + 支払手数料 440円
        String journalNo = "JE-20250102-001";
        LocalDate journalDate = LocalDate.of(2025, 1, 2);
        LocalDate inputDate = LocalDate.of(2025, 1, 2);

        // When: 仕訳を登録
        // 1. 仕訳ヘッダー
        String journalSql = """
            INSERT INTO "仕訳" (
                "仕訳伝票番号", "起票日", "入力日", "決算仕訳フラグ", "単振フラグ",
                "仕訳伝票区分", "定期計上フラグ", "赤伝フラグ"
            ) VALUES (?, ?, ?, 0, 0, 0, 0, 0)
            """;
        try (PreparedStatement pstmt = connection.prepareStatement(journalSql)) {
            pstmt.setString(1, journalNo);
            pstmt.setDate(2, Date.valueOf(journalDate));
            pstmt.setDate(3, Date.valueOf(inputDate));
            pstmt.executeUpdate();
        }

        // 2. 仕訳明細（2行）
        String detailSql = """
            INSERT INTO "仕訳明細" (
                "仕訳伝票番号", "仕訳行番号", "行摘要"
            ) VALUES (?, ?, ?)
            """;

        try (PreparedStatement pstmt = connection.prepareStatement(detailSql)) {
            pstmt.setString(1, journalNo);
            pstmt.setInt(2, 1);
            pstmt.setString(3, "売掛金回収（A社）");
            pstmt.executeUpdate();
        }

        try (PreparedStatement pstmt = connection.prepareStatement(detailSql)) {
            pstmt.setString(1, journalNo);
            pstmt.setInt(2, 2);
            pstmt.setString(3, "振込手数料");
            pstmt.executeUpdate();
        }

        // 3. 仕訳貸借明細（行1：普通預金/売掛金、行2：支払手数料/売掛金）
        String itemSql = """
            INSERT INTO "仕訳貸借明細" (
                "仕訳伝票番号", "仕訳行番号", "仕訳行貸借区分",
                "通貨コード", "為替レート", "勘定科目コード",
                "仕訳金額", "基軸換算仕訳金額", "資金繰フラグ"
            ) VALUES (?, ?, ?, 'JPY', 1.00, ?, ?, ?, ?)
            """;

        // 行1-借方: 普通預金 99,560円
        try (PreparedStatement pstmt = connection.prepareStatement(itemSql)) {
            pstmt.setString(1, journalNo);
            pstmt.setInt(2, 1);
            pstmt.setString(3, "D");
            pstmt.setString(4, "1200");
            pstmt.setBigDecimal(5, new BigDecimal("99560.00"));
            pstmt.setBigDecimal(6, new BigDecimal("99560.00"));
            pstmt.setInt(7, 1);
            pstmt.executeUpdate();
        }

        // 行1-貸方: 売掛金 99,560円
        try (PreparedStatement pstmt = connection.prepareStatement(itemSql)) {
            pstmt.setString(1, journalNo);
            pstmt.setInt(2, 1);
            pstmt.setString(3, "C");
            pstmt.setString(4, "1300");
            pstmt.setBigDecimal(5, new BigDecimal("99560.00"));
            pstmt.setBigDecimal(6, new BigDecimal("99560.00"));
            pstmt.setInt(7, 0);
            pstmt.executeUpdate();
        }

        // 行2-借方: 支払手数料 440円
        try (PreparedStatement pstmt = connection.prepareStatement(itemSql)) {
            pstmt.setString(1, journalNo);
            pstmt.setInt(2, 2);
            pstmt.setString(3, "D");
            pstmt.setString(4, "6200");
            pstmt.setBigDecimal(5, new BigDecimal("440.00"));
            pstmt.setBigDecimal(6, new BigDecimal("440.00"));
            pstmt.setInt(7, 0);
            pstmt.executeUpdate();
        }

        // 行2-貸方: 売掛金 440円
        try (PreparedStatement pstmt = connection.prepareStatement(itemSql)) {
            pstmt.setString(1, journalNo);
            pstmt.setInt(2, 2);
            pstmt.setString(3, "C");
            pstmt.setString(4, "1300");
            pstmt.setBigDecimal(5, new BigDecimal("440.00"));
            pstmt.setBigDecimal(6, new BigDecimal("440.00"));
            pstmt.setInt(7, 0);
            pstmt.executeUpdate();
        }

        // Then: データが正しく登録されていることを確認
        // 1. 仕訳明細が2件登録されている
        String detailCountSql = "SELECT COUNT(*) as count FROM \"仕訳明細\" WHERE \"仕訳伝票番号\" = ?";
        try (PreparedStatement pstmt = connection.prepareStatement(detailCountSql)) {
            pstmt.setString(1, journalNo);
            ResultSet rs = pstmt.executeQuery();
            rs.next();
            assertThat(rs.getInt("count")).isEqualTo(2);
        }

        // 2. 仕訳貸借明細が4件登録されている
        String itemCountSql = "SELECT COUNT(*) as count FROM \"仕訳貸借明細\" WHERE \"仕訳伝票番号\" = ?";
        try (PreparedStatement pstmt = connection.prepareStatement(itemCountSql)) {
            pstmt.setString(1, journalNo);
            ResultSet rs = pstmt.executeQuery();
            rs.next();
            assertThat(rs.getInt("count")).isEqualTo(4);
        }

        // 3. 借方・貸方の合計が一致する
        String balanceSql = """
            SELECT
                SUM(CASE WHEN "仕訳行貸借区分" = 'D' THEN "仕訳金額" ELSE 0 END) as debit_total,
                SUM(CASE WHEN "仕訳行貸借区分" = 'C' THEN "仕訳金額" ELSE 0 END) as credit_total
            FROM "仕訳貸借明細"
            WHERE "仕訳伝票番号" = ?
            """;
        try (PreparedStatement pstmt = connection.prepareStatement(balanceSql)) {
            pstmt.setString(1, journalNo);
            ResultSet rs = pstmt.executeQuery();
            rs.next();

            BigDecimal debitTotal = rs.getBigDecimal("debit_total");
            BigDecimal creditTotal = rs.getBigDecimal("credit_total");

            assertThat(debitTotal).isEqualByComparingTo(creditTotal);
            assertThat(debitTotal).isEqualByComparingTo(new BigDecimal("100000.00"));
        }

        // 4. 単振フラグが0（複合仕訳）になっている
        String flagSql = "SELECT \"単振フラグ\" as tanpu_flag FROM \"仕訳\" WHERE \"仕訳伝票番号\" = ?";
        try (PreparedStatement pstmt = connection.prepareStatement(flagSql)) {
            pstmt.setString(1, journalNo);
            ResultSet rs = pstmt.executeQuery();
            rs.next();
            assertThat(rs.getInt("tanpu_flag")).isEqualTo(0);
        }
    }

    @Test
    @Order(3)
    @DisplayName("外部キー制約_仕訳削除時に明細も削除される")
    void test_外部キー制約_仕訳削除時に明細も削除される() throws SQLException {
        // Given: 仕訳を登録
        String journalNo = "JE-20250103-001";
        LocalDate journalDate = LocalDate.of(2025, 1, 3);

        // 仕訳ヘッダー
        String journalSql = """
            INSERT INTO "仕訳" (
                "仕訳伝票番号", "起票日", "入力日", "決算仕訳フラグ", "単振フラグ",
                "仕訳伝票区分", "定期計上フラグ", "赤伝フラグ"
            ) VALUES (?, ?, ?, 0, 1, 0, 0, 0)
            """;
        try (PreparedStatement pstmt = connection.prepareStatement(journalSql)) {
            pstmt.setString(1, journalNo);
            pstmt.setDate(2, Date.valueOf(journalDate));
            pstmt.setDate(3, Date.valueOf(journalDate));
            pstmt.executeUpdate();
        }

        // 仕訳明細
        String detailSql = """
            INSERT INTO "仕訳明細" (
                "仕訳伝票番号", "仕訳行番号", "行摘要"
            ) VALUES (?, 1, 'テスト')
            """;
        try (PreparedStatement pstmt = connection.prepareStatement(detailSql)) {
            pstmt.setString(1, journalNo);
            pstmt.executeUpdate();
        }

        // 仕訳貸借明細
        String itemSql = """
            INSERT INTO "仕訳貸借明細" (
                "仕訳伝票番号", "仕訳行番号", "仕訳行貸借区分",
                "通貨コード", "為替レート", "勘定科目コード",
                "仕訳金額", "基軸換算仕訳金額", "資金繰フラグ"
            ) VALUES (?, 1, 'D', 'JPY', 1.00, '1100', 10000.00, 10000.00, 0)
            """;
        try (PreparedStatement pstmt = connection.prepareStatement(itemSql)) {
            pstmt.setString(1, journalNo);
            pstmt.executeUpdate();
        }

        // When: 仕訳を削除
        String deleteSql = "DELETE FROM \"仕訳\" WHERE \"仕訳伝票番号\" = ?";
        try (PreparedStatement pstmt = connection.prepareStatement(deleteSql)) {
            pstmt.setString(1, journalNo);
            pstmt.executeUpdate();
        }

        // Then: 明細と貸借明細も自動削除される（CASCADE）
        String detailCountSql = "SELECT COUNT(*) as count FROM \"仕訳明細\" WHERE \"仕訳伝票番号\" = ?";
        try (PreparedStatement pstmt = connection.prepareStatement(detailCountSql)) {
            pstmt.setString(1, journalNo);
            ResultSet rs = pstmt.executeQuery();
            rs.next();
            assertThat(rs.getInt("count")).isEqualTo(0);
        }

        String itemCountSql = "SELECT COUNT(*) as count FROM \"仕訳貸借明細\" WHERE \"仕訳伝票番号\" = ?";
        try (PreparedStatement pstmt = connection.prepareStatement(itemCountSql)) {
            pstmt.setString(1, journalNo);
            ResultSet rs = pstmt.executeQuery();
            rs.next();
            assertThat(rs.getInt("count")).isEqualTo(0);
        }
    }

    /**
     * テスト用勘定科目を登録するヘルパーメソッド
     */
    private void insertTestAccounts() throws SQLException {
        String sql = """
            INSERT INTO "勘定科目マスタ" ("勘定科目コード", "勘定科目名", "勘定科目種別", "合計科目", "集計対象", "残高")
            VALUES
            ('1100', '現金', '資産'::account_type, false, true, 0),
            ('1200', '普通預金', '資産'::account_type, false, true, 0),
            ('1300', '売掛金', '資産'::account_type, false, true, 0),
            ('5110', '仕入', '費用'::account_type, false, true, 0),
            ('6200', '支払手数料', '費用'::account_type, false, true, 0)
            """;

        try (Statement stmt = connection.createStatement()) {
            stmt.executeUpdate(sql);
        }
    }
}
