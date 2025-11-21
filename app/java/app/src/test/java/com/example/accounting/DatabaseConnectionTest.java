package com.example.accounting;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jdbc.core.JdbcTemplate;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * Testcontainers データベース接続テスト
 */
@DisplayName("Testcontainers データベース接続テスト")
class DatabaseConnectionTest extends TestDatabaseConfig {

    @Autowired
    private JdbcTemplate jdbcTemplate;

    @Test
    @DisplayName("データベースに接続できる")
    void testDatabaseConnection() {
        // 接続確認用の簡単なクエリ
        Integer result = jdbcTemplate.queryForObject("SELECT 1", Integer.class);
        assertThat(result).isEqualTo(1);
    }

    @Test
    @DisplayName("テーブルが作成されている")
    void testTableExists() {
        // 勘定科目マスタテーブルの存在確認
        Boolean exists = jdbcTemplate.queryForObject(
            "SELECT EXISTS ("
            + "  SELECT FROM information_schema.tables "
            + "  WHERE table_schema = 'public' "
            + "  AND table_name = '勘定科目マスタ'"
            + ")",
            Boolean.class
        );
        assertThat(exists).isTrue();
    }

    @Test
    @DisplayName("テスト間でデータがクリアされる")
    void testDataCleanup() {
        // 最初のテストデータ挿入
        jdbcTemplate.update(
            "INSERT INTO \"勘定科目マスタ\" (\"勘定科目コード\", \"勘定科目名\", \"勘定科目種別\", \"残高\") "
            + "VALUES (?, ?, ?::account_type, ?)",
            "1000", "現金", "資産", 50000
        );

        // データが挿入されていることを確認
        Integer count1 = jdbcTemplate.queryForObject(
            "SELECT COUNT(*) FROM \"勘定科目マスタ\"",
            Integer.class
        );
        assertThat(count1).isEqualTo(1);

        // データをクリア
        jdbcTemplate.update("TRUNCATE TABLE \"勘定科目マスタ\" CASCADE");

        // データがクリアされていることを確認
        Integer count2 = jdbcTemplate.queryForObject(
            "SELECT COUNT(*) FROM \"勘定科目マスタ\"",
            Integer.class
        );
        assertThat(count2).isEqualTo(0);
    }
}
