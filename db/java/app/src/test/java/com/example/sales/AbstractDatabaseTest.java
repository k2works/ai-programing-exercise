package com.example.sales;

import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.DynamicPropertyRegistry;
import org.springframework.test.context.DynamicPropertySource;
import org.testcontainers.containers.PostgreSQLContainer;

/**
 * データベーステスト用の基底クラス
 * Testcontainersを使用してPostgreSQLコンテナを起動し、テストを実行する
 *
 * このクラスを継承することで、以下の機能が利用できます：
 * - PostgreSQLコンテナの自動起動・停止
 * - Flywayマイグレーションの自動実行
 * - Spring Bootアプリケーションコンテキストの起動
 * - JdbcTemplateやMyBatisマッパーの自動注入
 */
@SpringBootTest
@ActiveProfiles("test")
public abstract class AbstractDatabaseTest {

    /**
     * PostgreSQLコンテナの定義
     * すべてのテストクラスで1つのコンテナを共有するため、static初期化ブロックで起動
     * withReuse(true)により、テスト実行間でコンテナを再利用
     */
    private static final PostgreSQLContainer<?> postgres;

    static {
        postgres = new PostgreSQLContainer<>("postgres:16-alpine")
            .withDatabaseName("sales_management_test")
            .withUsername("test")
            .withPassword("test")
            .withReuse(true);
        postgres.start();
    }

    /**
     * Testcontainersが起動したコンテナの接続情報をSpring Bootに動的に設定
     * コンテナのポートはランダムに割り当てられるため、動的に設定する必要があります
     */
    @DynamicPropertySource
    static void configureProperties(DynamicPropertyRegistry registry) {
        registry.add("spring.datasource.url", postgres::getJdbcUrl);
        registry.add("spring.datasource.username", postgres::getUsername);
        registry.add("spring.datasource.password", postgres::getPassword);
    }
}
