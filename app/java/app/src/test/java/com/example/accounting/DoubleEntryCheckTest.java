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
 * 複式簿記チェック機能のテスト
 *
 * PostgreSQL のビューと関数を使った複式簿記の原理（借方合計 = 貸方合計）の検証
 */
@Testcontainers
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
class DoubleEntryCheckTest {

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
            stmt.execute("TRUNCATE TABLE \"仕訳\" CASCADE");
            stmt.execute("TRUNCATE TABLE \"勘定科目マスタ\" CASCADE");
        }

        insertTestAccounts();
    }

    @Test
    @Order(1)
    @DisplayName("借方・貸方が一致する仕訳は不整合として検出されない")
    void testBalancedJournalIsNotDetected() throws SQLException {
        // Given: 借方・貸方が一致する仕訳を登録
        String journalNo = "JE-20250101-001";
        insertBalancedJournal(journalNo);

        // When: 複式簿記チェック関数を実行
        String checkSql = "SELECT * FROM \"複式簿記チェック\"()";
        try (PreparedStatement pstmt = connection.prepareStatement(checkSql)) {
            ResultSet rs = pstmt.executeQuery();

            // Then: 不整合な仕訳は検出されない
            assertThat(rs.next()).isFalse();
        }
    }

    @Test
    @Order(2)
    @DisplayName("借方・貸方が不一致の仕訳は不整合として検出される")
    void testUnbalancedJournalIsDetected() throws SQLException {
        // Given: 借方・貸方が不一致の仕訳を登録
        String journalNo = "JE-20250102-001";
        insertUnbalancedJournal(journalNo);

        // When: 複式簿記チェック関数を実行
        String checkSql = "SELECT * FROM \"複式簿記チェック\"()";
        try (PreparedStatement pstmt = connection.prepareStatement(checkSql)) {
            ResultSet rs = pstmt.executeQuery();

            // Then: 不整合な仕訳が検出される
            assertThat(rs.next()).isTrue();
            assertThat(rs.getString("不整合伝票番号")).isEqualTo(journalNo);
            assertThat(rs.getBigDecimal("差額")).isEqualByComparingTo(new BigDecimal("1000.00"));
        }
    }

    @Test
    @Order(3)
    @DisplayName("仕訳残高チェックビューで借方・貸方の合計を確認できる")
    void testJournalBalanceView() throws SQLException {
        // Given: 仕訳を登録
        String journalNo = "JE-20250103-001";
        insertBalancedJournal(journalNo);

        // When: 仕訳残高チェックビューを参照
        String viewSql = """
            SELECT "仕訳伝票番号", "借方合計", "貸方合計", "差額"
            FROM "仕訳残高チェック"
            WHERE "仕訳伝票番号" = ?
            """;
        try (PreparedStatement pstmt = connection.prepareStatement(viewSql)) {
            pstmt.setString(1, journalNo);
            ResultSet rs = pstmt.executeQuery();

            // Then: 借方・貸方の合計が正しく計算されている
            assertThat(rs.next()).isTrue();
            assertThat(rs.getBigDecimal("借方合計")).isEqualByComparingTo(new BigDecimal("100000.00"));
            assertThat(rs.getBigDecimal("貸方合計")).isEqualByComparingTo(new BigDecimal("100000.00"));
            assertThat(rs.getBigDecimal("差額")).isEqualByComparingTo(BigDecimal.ZERO);
        }
    }

    @Test
    @Order(4)
    @DisplayName("複数の仕訳のうち不整合のものだけ検出される")
    void testMultipleJournalsWithOneUnbalanced() throws SQLException {
        // Given: 正常な仕訳2件と不整合な仕訳1件を登録
        insertBalancedJournal("JE-20250104-001");
        insertBalancedJournal("JE-20250104-002");
        insertUnbalancedJournal("JE-20250104-003");

        // When: 複式簿記チェック関数を実行
        String checkSql = "SELECT * FROM \"複式簿記チェック\"()";
        try (PreparedStatement pstmt = connection.prepareStatement(checkSql)) {
            ResultSet rs = pstmt.executeQuery();

            // Then: 不整合な仕訳だけが検出される
            assertThat(rs.next()).isTrue();
            assertThat(rs.getString("不整合伝票番号")).isEqualTo("JE-20250104-003");

            // 2件目はない
            assertThat(rs.next()).isFalse();
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
            ('1200', '普通預金', '資産'::account_type, false, true, 0),
            ('5110', '仕入', '費用'::account_type, false, true, 0),
            ('4100', '売上', '収益'::account_type, false, true, 0)
            """;

        try (Statement stmt = connection.createStatement()) {
            stmt.executeUpdate(sql);
        }
    }

    /**
     * 借方・貸方が一致する正常な仕訳を登録
     */
    private void insertBalancedJournal(String journalNo) throws SQLException {
        // 仕訳ヘッダー
        String journalSql = """
            INSERT INTO "仕訳" (
                "仕訳伝票番号", "起票日", "入力日", "決算仕訳フラグ", "単振フラグ",
                "仕訳伝票区分", "定期計上フラグ", "赤伝フラグ"
            ) VALUES (?, ?, ?, 0, 1, 0, 0, 0)
            """;
        try (PreparedStatement pstmt = connection.prepareStatement(journalSql)) {
            pstmt.setString(1, journalNo);
            pstmt.setDate(2, Date.valueOf(LocalDate.of(2025, 1, 1)));
            pstmt.setDate(3, Date.valueOf(LocalDate.of(2025, 1, 1)));
            pstmt.executeUpdate();
        }

        // 仕訳明細
        String detailSql = """
            INSERT INTO "仕訳明細" (
                "仕訳伝票番号", "仕訳行番号", "行摘要"
            ) VALUES (?, 1, '商品仕入')
            """;
        try (PreparedStatement pstmt = connection.prepareStatement(detailSql)) {
            pstmt.setString(1, journalNo);
            pstmt.executeUpdate();
        }

        // 仕訳貸借明細（借方：仕入 100,000、貸方：現金 100,000）
        String itemSql = """
            INSERT INTO "仕訳貸借明細" (
                "仕訳伝票番号", "仕訳行番号", "仕訳行貸借区分",
                "通貨コード", "為替レート", "勘定科目コード",
                "仕訳金額", "基軸換算仕訳金額", "資金繰フラグ"
            ) VALUES (?, 1, ?, 'JPY', 1.00, ?, ?, ?, 0)
            """;

        // 借方
        try (PreparedStatement pstmt = connection.prepareStatement(itemSql)) {
            pstmt.setString(1, journalNo);
            pstmt.setString(2, "D");
            pstmt.setString(3, "5110");
            pstmt.setBigDecimal(4, new BigDecimal("100000.00"));
            pstmt.setBigDecimal(5, new BigDecimal("100000.00"));
            pstmt.executeUpdate();
        }

        // 貸方
        try (PreparedStatement pstmt = connection.prepareStatement(itemSql)) {
            pstmt.setString(1, journalNo);
            pstmt.setString(2, "C");
            pstmt.setString(3, "1100");
            pstmt.setBigDecimal(4, new BigDecimal("100000.00"));
            pstmt.setBigDecimal(5, new BigDecimal("100000.00"));
            pstmt.executeUpdate();
        }
    }

    /**
     * 借方・貸方が不一致の異常な仕訳を登録（テスト用）
     */
    private void insertUnbalancedJournal(String journalNo) throws SQLException {
        // 仕訳ヘッダー
        String journalSql = """
            INSERT INTO "仕訳" (
                "仕訳伝票番号", "起票日", "入力日", "決算仕訳フラグ", "単振フラグ",
                "仕訳伝票区分", "定期計上フラグ", "赤伝フラグ"
            ) VALUES (?, ?, ?, 0, 1, 0, 0, 0)
            """;
        try (PreparedStatement pstmt = connection.prepareStatement(journalSql)) {
            pstmt.setString(1, journalNo);
            pstmt.setDate(2, Date.valueOf(LocalDate.of(2025, 1, 2)));
            pstmt.setDate(3, Date.valueOf(LocalDate.of(2025, 1, 2)));
            pstmt.executeUpdate();
        }

        // 仕訳明細
        String detailSql = """
            INSERT INTO "仕訳明細" (
                "仕訳伝票番号", "仕訳行番号", "行摘要"
            ) VALUES (?, 1, 'テスト（不整合）')
            """;
        try (PreparedStatement pstmt = connection.prepareStatement(detailSql)) {
            pstmt.setString(1, journalNo);
            pstmt.executeUpdate();
        }

        // 仕訳貸借明細（借方：仕入 101,000、貸方：現金 100,000 → 差額 1,000）
        String itemSql = """
            INSERT INTO "仕訳貸借明細" (
                "仕訳伝票番号", "仕訳行番号", "仕訳行貸借区分",
                "通貨コード", "為替レート", "勘定科目コード",
                "仕訳金額", "基軸換算仕訳金額", "資金繰フラグ"
            ) VALUES (?, 1, ?, 'JPY', 1.00, ?, ?, ?, 0)
            """;

        // 借方（101,000 - 意図的に不一致）
        try (PreparedStatement pstmt = connection.prepareStatement(itemSql)) {
            pstmt.setString(1, journalNo);
            pstmt.setString(2, "D");
            pstmt.setString(3, "5110");
            pstmt.setBigDecimal(4, new BigDecimal("101000.00"));
            pstmt.setBigDecimal(5, new BigDecimal("101000.00"));
            pstmt.executeUpdate();
        }

        // 貸方（100,000）
        try (PreparedStatement pstmt = connection.prepareStatement(itemSql)) {
            pstmt.setString(1, journalNo);
            pstmt.setString(2, "C");
            pstmt.setString(3, "1100");
            pstmt.setBigDecimal(4, new BigDecimal("100000.00"));
            pstmt.setBigDecimal(5, new BigDecimal("100000.00"));
            pstmt.executeUpdate();
        }
    }
}
