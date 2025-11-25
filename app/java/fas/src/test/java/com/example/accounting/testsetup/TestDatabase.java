package com.example.accounting.testsetup;

import org.flywaydb.core.Flyway;
import org.testcontainers.containers.PostgreSQLContainer;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.sql.Statement;

/**
 * テスト用データベース管理クラス
 *
 * TestContainersでPostgreSQLコンテナを起動し、
 * Flywayマイグレーションを実行してテーブルを作成します。
 */
public class TestDatabase {

    private final PostgreSQLContainer<?> container;
    private Connection connection;

    public TestDatabase(PostgreSQLContainer<?> container) {
        this.container = container;
    }

    /**
     * データベース接続を初期化し、Flywayマイグレーションを実行
     */
    public void start() throws SQLException {
        connection = DriverManager.getConnection(
                container.getJdbcUrl(),
                container.getUsername(),
                container.getPassword()
        );
        runMigrations();
    }

    /**
     * データベース接続をクローズ
     */
    public void stop() throws SQLException {
        if (connection != null && !connection.isClosed()) {
            connection.close();
        }
    }

    /**
     * すべてのテーブルのデータをクリア
     */
    public void cleanup() throws SQLException {
        try (Statement stmt = connection.createStatement()) {
            stmt.execute("TRUNCATE TABLE \"勘定科目マスタ\" CASCADE");
        }
    }

    /**
     * データベース接続を取得
     */
    public Connection getConnection() {
        return connection;
    }

    /**
     * Flywayマイグレーション実行
     */
    private void runMigrations() {
        Flyway flyway = Flyway.configure()
                .dataSource(container.getJdbcUrl(), container.getUsername(), container.getPassword())
                .locations("classpath:db/migration")
                .load();
        flyway.migrate();
    }
}
