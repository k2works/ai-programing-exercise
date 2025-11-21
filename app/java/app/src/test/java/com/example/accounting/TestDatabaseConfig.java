package com.example.accounting;

import org.apache.ibatis.io.Resources;
import org.apache.ibatis.session.SqlSessionFactory;
import org.apache.ibatis.session.SqlSessionFactoryBuilder;
import org.flywaydb.core.Flyway;
import org.junit.jupiter.api.BeforeAll;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;

import java.io.InputStream;
import java.util.Properties;

/**
 * Testcontainersを使用したMyBatisテストの基底クラス
 *
 * 全てのMapperTestクラスで共通のセットアップ処理を提供します。
 */
@Testcontainers
public abstract class TestDatabaseConfig {

    @Container
    protected static final PostgreSQLContainer<?> postgres =
        new PostgreSQLContainer<>("postgres:16-alpine")
            .withDatabaseName("testdb")
            .withUsername("testuser")
            .withPassword("testpass");

    protected static SqlSessionFactory sqlSessionFactory;

    @BeforeAll
    static void setUpDatabase() throws Exception {
        // Flyway マイグレーション実行
        Flyway flyway = Flyway.configure()
                .dataSource(postgres.getJdbcUrl(), postgres.getUsername(), postgres.getPassword())
                .locations("classpath:db/migration")
                .load();
        flyway.migrate();

        // MyBatis セットアップ
        Properties properties = new Properties();
        properties.setProperty("driver", "org.postgresql.Driver");
        properties.setProperty("url", postgres.getJdbcUrl());
        properties.setProperty("username", postgres.getUsername());
        properties.setProperty("password", postgres.getPassword());

        InputStream inputStream = Resources.getResourceAsStream("mybatis-config.xml");
        sqlSessionFactory = new SqlSessionFactoryBuilder().build(inputStream, properties);
    }
}
