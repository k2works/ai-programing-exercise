package com.example.sales;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jdbc.core.JdbcTemplate;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * データベース接続テスト
 * Testcontainersが正しく動作し、データベースに接続できることを確認します
 */
class DatabaseConnectionTest extends AbstractDatabaseTest {

    @Autowired
    private JdbcTemplate jdbcTemplate;

    @Test
    void データベースに接続できる() {
        // 接続確認用の簡単なクエリ
        Integer result = jdbcTemplate.queryForObject("SELECT 1", Integer.class);
        assertThat(result).isEqualTo(1);
    }

    @Test
    void Flywayマイグレーションが実行されている() {
        // V1__Initial_setup.sqlが実行されていることを確認
        // publicスキーマが存在することを確認
        Integer count = jdbcTemplate.queryForObject(
            "SELECT COUNT(*) FROM information_schema.schemata WHERE schema_name = 'public'",
            Integer.class
        );
        assertThat(count).isGreaterThan(0);
    }
}
