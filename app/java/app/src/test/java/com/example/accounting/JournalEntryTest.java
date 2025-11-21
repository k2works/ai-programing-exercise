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
 * 仕訳エントリのテスト
 *
 * このテストでは、仕訳エントリに対するCRUD操作と複式簿記の原理を検証します。
 * TestContainersを使用して、実際のPostgreSQLコンテナでテストを実行します。
 */
@Testcontainers
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
class JournalEntryTest {

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
        // 各テスト前にデータをクリア
        try (Statement stmt = connection.createStatement()) {
            stmt.execute("TRUNCATE TABLE \"仕訳エントリ\" CASCADE");
            stmt.execute("TRUNCATE TABLE \"勘定科目マスタ\" CASCADE");
        }

        // テスト用勘定科目を登録
        insertTestAccounts();
    }

    @Test
    @Order(1)
    @DisplayName("仕訳エントリを登録できる")
    void testCreateJournalEntry() throws SQLException {
        // 1. 仕訳エントリを作成
        String entrySql = """
            INSERT INTO "仕訳エントリ" ("伝票番号", "仕訳日", "摘要", "合計金額", "作成者")
            VALUES (?, ?, ?, ?, ?)
            RETURNING "伝票番号"
            """;

        String entryNo;
        try (PreparedStatement pstmt = connection.prepareStatement(entrySql)) {
            pstmt.setString(1, "JE240001");
            pstmt.setDate(2, Date.valueOf(LocalDate.of(2024, 1, 15)));
            pstmt.setString(3, "現金売上");
            pstmt.setBigDecimal(4, new BigDecimal("110000.00"));
            pstmt.setString(5, "user001");

            ResultSet rs = pstmt.executeQuery();
            assertThat(rs.next()).isTrue();
            entryNo = rs.getString("伝票番号");
            assertThat(entryNo).isEqualTo("JE240001");
        }

        // 2. 仕訳明細を作成（借方：現金、貸方：売上+消費税）
        String detailSql = """
            INSERT INTO "仕訳明細" (
                "伝票番号", "行番号", "勘定科目コード",
                "借方金額", "貸方金額", "摘要"
            ) VALUES (?, ?, ?, ?, ?, ?)
            """;

        // 借方：現金 110,000
        try (PreparedStatement pstmt = connection.prepareStatement(detailSql)) {
            pstmt.setString(1, entryNo);
            pstmt.setInt(2, 1);
            pstmt.setString(3, "1100"); // 現金
            pstmt.setBigDecimal(4, new BigDecimal("110000.00"));
            pstmt.setBigDecimal(5, BigDecimal.ZERO);
            pstmt.setString(6, "商品売上による現金収入");
            pstmt.executeUpdate();
        }

        // 貸方：売上 100,000
        try (PreparedStatement pstmt = connection.prepareStatement(detailSql)) {
            pstmt.setString(1, entryNo);
            pstmt.setInt(2, 2);
            pstmt.setString(3, "4100"); // 売上
            pstmt.setBigDecimal(4, BigDecimal.ZERO);
            pstmt.setBigDecimal(5, new BigDecimal("100000.00"));
            pstmt.setString(6, "商品売上");
            pstmt.executeUpdate();
        }

        // 貸方：仮受消費税 10,000
        try (PreparedStatement pstmt = connection.prepareStatement(detailSql)) {
            pstmt.setString(1, entryNo);
            pstmt.setInt(2, 3);
            pstmt.setString(3, "2120"); // 仮受消費税
            pstmt.setBigDecimal(4, BigDecimal.ZERO);
            pstmt.setBigDecimal(5, new BigDecimal("10000.00"));
            pstmt.setString(6, "消費税");
            pstmt.executeUpdate();
        }

        // 3. 借方・貸方の合計を検証（複式簿記の原理）
        String balanceSql = """
            SELECT
                SUM("借方金額") as debit_total,
                SUM("貸方金額") as credit_total
            FROM "仕訳明細"
            WHERE "伝票番号" = ?
            """;

        try (PreparedStatement pstmt = connection.prepareStatement(balanceSql)) {
            pstmt.setString(1, entryNo);
            ResultSet rs = pstmt.executeQuery();

            assertThat(rs.next()).isTrue();
            BigDecimal debitTotal = rs.getBigDecimal("debit_total");
            BigDecimal creditTotal = rs.getBigDecimal("credit_total");

            // 複式簿記の原理：借方合計 = 貸方合計
            assertThat(debitTotal).isEqualByComparingTo(creditTotal);
            assertThat(debitTotal).isEqualByComparingTo(new BigDecimal("110000.00"));
        }
    }

    @Test
    @Order(2)
    @DisplayName("仕訳エントリを更新できる")
    void testUpdateJournalEntry() throws SQLException {
        // 1. テストデータを登録
        String entryNo = insertJournalEntry(
            "JE240001",
            LocalDate.of(2024, 1, 15),
            "現金売上",
            new BigDecimal("110000.00"),
            "user001"
        );

        // 2. 摘要を更新
        String updateSql = """
            UPDATE "仕訳エントリ"
            SET "摘要" = ?, "更新者" = ?, "更新日時" = CURRENT_TIMESTAMP
            WHERE "伝票番号" = ?
            """;

        try (PreparedStatement pstmt = connection.prepareStatement(updateSql)) {
            pstmt.setString(1, "現金売上（修正）");
            pstmt.setString(2, "user002");
            pstmt.setString(3, entryNo);
            int updated = pstmt.executeUpdate();
            assertThat(updated).isEqualTo(1);
        }

        // 3. 更新されたか検証
        String selectSql = """
            SELECT "摘要", "更新者"
            FROM "仕訳エントリ"
            WHERE "伝票番号" = ?
            """;

        try (PreparedStatement pstmt = connection.prepareStatement(selectSql)) {
            pstmt.setString(1, entryNo);
            ResultSet rs = pstmt.executeQuery();

            assertThat(rs.next()).isTrue();
            assertThat(rs.getString("摘要")).isEqualTo("現金売上（修正）");
            assertThat(rs.getString("更新者")).isEqualTo("user002");
        }
    }

    @Test
    @Order(3)
    @DisplayName("仕訳エントリを削除できる（明細も連鎖削除）")
    void testDeleteJournalEntry() throws SQLException {
        // 1. テストデータを登録
        String entryNo = insertJournalEntry(
            "JE240001",
            LocalDate.of(2024, 1, 15),
            "現金売上",
            new BigDecimal("110000.00"),
            "user001"
        );

        // 仕訳明細も登録
        insertJournalDetail(entryNo, 1, "1100", new BigDecimal("110000"), BigDecimal.ZERO, "現金");
        insertJournalDetail(entryNo, 2, "4100", BigDecimal.ZERO, new BigDecimal("110000"), "売上");

        // 2. 仕訳エントリを削除
        String deleteSql = """
            DELETE FROM "仕訳エントリ"
            WHERE "伝票番号" = ?
            """;

        try (PreparedStatement pstmt = connection.prepareStatement(deleteSql)) {
            pstmt.setString(1, entryNo);
            int deleted = pstmt.executeUpdate();
            assertThat(deleted).isEqualTo(1);
        }

        // 3. データが削除されたか検証
        String checkEntrySql = "SELECT COUNT(*) as count FROM \"仕訳エントリ\" WHERE \"伝票番号\" = ?";
        String checkDetailSql = "SELECT COUNT(*) as count FROM \"仕訳明細\" WHERE \"伝票番号\" = ?";

        try (PreparedStatement pstmt = connection.prepareStatement(checkEntrySql)) {
            pstmt.setString(1, entryNo);
            ResultSet rs = pstmt.executeQuery();
            rs.next();
            assertThat(rs.getInt("count")).isEqualTo(0);
        }

        // 明細も連鎖削除されているか確認
        try (PreparedStatement pstmt = connection.prepareStatement(checkDetailSql)) {
            pstmt.setString(1, entryNo);
            ResultSet rs = pstmt.executeQuery();
            rs.next();
            assertThat(rs.getInt("count")).isEqualTo(0);
        }
    }

    @Test
    @Order(4)
    @DisplayName("複雑な仕訳エントリ（売掛金回収と振込手数料）を登録できる")
    void testComplexJournalEntry() throws SQLException {
        // 1. 仕訳エントリを作成
        String entryNo = insertJournalEntry(
            "JE240002",
            LocalDate.of(2024, 1, 20),
            "売掛金回収と振込手数料",
            new BigDecimal("105000.00"),
            "user001"
        );

        // 2. 複雑な仕訳明細を作成
        // 借方：普通預金 104,500（振込手数料差引後）
        insertJournalDetail(entryNo, 1, "1200", new BigDecimal("104500.00"), BigDecimal.ZERO,
            "売掛金回収（振込手数料差引後）");

        // 借方：支払手数料 500
        insertJournalDetail(entryNo, 2, "6200", new BigDecimal("500.00"), BigDecimal.ZERO,
            "振込手数料");

        // 貸方：売掛金 105,000
        insertJournalDetail(entryNo, 3, "1300", BigDecimal.ZERO, new BigDecimal("105000.00"),
            "売掛金回収");

        // 3. 借方・貸方の合計を検証
        String balanceSql = """
            SELECT
                SUM("借方金額") as debit_total,
                SUM("貸方金額") as credit_total
            FROM "仕訳明細"
            WHERE "伝票番号" = ?
            """;

        try (PreparedStatement pstmt = connection.prepareStatement(balanceSql)) {
            pstmt.setString(1, entryNo);
            ResultSet rs = pstmt.executeQuery();

            assertThat(rs.next()).isTrue();
            BigDecimal debitTotal = rs.getBigDecimal("debit_total");
            BigDecimal creditTotal = rs.getBigDecimal("credit_total");

            // 複式簿記の原理：借方合計 = 貸方合計
            assertThat(debitTotal).isEqualByComparingTo(creditTotal);
            assertThat(debitTotal).isEqualByComparingTo(new BigDecimal("105000.00"));
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
            ('2120', '仮受消費税', '負債'::account_type, false, true, 0),
            ('4100', '売上', '収益'::account_type, false, true, 0),
            ('6200', '支払手数料', '費用'::account_type, false, true, 0)
            """;

        try (Statement stmt = connection.createStatement()) {
            stmt.executeUpdate(sql);
        }
    }

    /**
     * 仕訳エントリを登録するヘルパーメソッド
     */
    private String insertJournalEntry(String entryNo, LocalDate entryDate, String description,
                                      BigDecimal totalAmount, String createdBy) throws SQLException {
        String sql = """
            INSERT INTO "仕訳エントリ" ("伝票番号", "仕訳日", "摘要", "合計金額", "作成者")
            VALUES (?, ?, ?, ?, ?)
            """;

        try (PreparedStatement pstmt = connection.prepareStatement(sql)) {
            pstmt.setString(1, entryNo);
            pstmt.setDate(2, Date.valueOf(entryDate));
            pstmt.setString(3, description);
            pstmt.setBigDecimal(4, totalAmount);
            pstmt.setString(5, createdBy);
            pstmt.executeUpdate();
        }

        return entryNo;
    }

    /**
     * 仕訳明細を登録するヘルパーメソッド
     */
    private void insertJournalDetail(String entryNo, int lineNo, String accountCode,
                                      BigDecimal debitAmount, BigDecimal creditAmount,
                                      String description) throws SQLException {
        String sql = """
            INSERT INTO "仕訳明細" (
                "伝票番号", "行番号", "勘定科目コード",
                "借方金額", "貸方金額", "摘要"
            ) VALUES (?, ?, ?, ?, ?, ?)
            """;

        try (PreparedStatement pstmt = connection.prepareStatement(sql)) {
            pstmt.setString(1, entryNo);
            pstmt.setInt(2, lineNo);
            pstmt.setString(3, accountCode);
            pstmt.setBigDecimal(4, debitAmount);
            pstmt.setBigDecimal(5, creditAmount);
            pstmt.setString(6, description);
            pstmt.executeUpdate();
        }
    }
}
