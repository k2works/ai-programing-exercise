package com.example.production.testsetup;

import org.flywaydb.core.Flyway;
import org.springframework.boot.jdbc.DataSourceBuilder;
import org.testcontainers.containers.PostgreSQLContainer;

import javax.sql.DataSource;

public class TestDatabase {

    private static PostgreSQLContainer<?> postgres;
    private static DataSource dataSource;

    public static void start() {
        if (postgres == null) {
            postgres = new PostgreSQLContainer<>("postgres:16-alpine")
                    .withDatabaseName("testdb")
                    .withUsername("testuser")
                    .withPassword("testpass");
            postgres.start();

            dataSource = DataSourceBuilder.create()
                    .url(postgres.getJdbcUrl())
                    .username(postgres.getUsername())
                    .password(postgres.getPassword())
                    .driverClassName("org.postgresql.Driver")
                    .build();

            // Flyway マイグレーション実行
            Flyway flyway = Flyway.configure()
                    .dataSource(dataSource)
                    .locations("classpath:db/migration")
                    .load();
            flyway.migrate();
        }
    }

    public static void stop() {
        if (postgres != null) {
            postgres.stop();
            postgres = null;
        }
    }

    public static DataSource getDataSource() {
        return dataSource;
    }

    public static String getJdbcUrl() {
        return postgres.getJdbcUrl();
    }

    public static String getUsername() {
        return postgres.getUsername();
    }

    public static String getPassword() {
        return postgres.getPassword();
    }
}
