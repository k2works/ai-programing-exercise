package com.example.accounting;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.web.client.TestRestTemplate;
import org.springframework.context.ApplicationContext;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.DynamicPropertyRegistry;
import org.springframework.test.context.DynamicPropertySource;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * アプリケーションスモークテスト
 *
 * Testcontainers を使用して実際の PostgreSQL コンテナで統合テストを実行
 */
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@Testcontainers
@DisplayName("アプリケーション - スモークテスト")
class AppTest {

    @Container
    static PostgreSQLContainer<?> postgres = new PostgreSQLContainer<>("postgres:15-alpine")
            .withDatabaseName("accounting_test")
            .withUsername("test")
            .withPassword("test");

    @DynamicPropertySource
    static void configureProperties(DynamicPropertyRegistry registry) {
        registry.add("spring.datasource.url", postgres::getJdbcUrl);
        registry.add("spring.datasource.username", postgres::getUsername);
        registry.add("spring.datasource.password", postgres::getPassword);
    }

    @Autowired
    private ApplicationContext applicationContext;

    @Autowired
    private TestRestTemplate restTemplate;

    @Test
    @DisplayName("アプリケーションコンテキストが正常にロードされる")
    void contextLoads() {
        assertThat(applicationContext).isNotNull();
    }

    @Test
    @DisplayName("必要な Bean が登録されている")
    void requiredBeansAreRegistered() {
        // Service beans
        assertThat(applicationContext.containsBean("accountService")).isTrue();
        assertThat(applicationContext.containsBean("journalService")).isTrue();
        assertThat(applicationContext.containsBean("auditLogService")).isTrue();
        assertThat(applicationContext.containsBean("balanceService")).isTrue();
        assertThat(applicationContext.containsBean("financialStatementService")).isTrue();
        assertThat(applicationContext.containsBean("journalEntryEventSourcingService")).isTrue();
        assertThat(applicationContext.containsBean("journalEntryQueryService")).isTrue();

        // Controller beans
        assertThat(applicationContext.containsBean("accountController")).isTrue();
        assertThat(applicationContext.containsBean("journalController")).isTrue();
        assertThat(applicationContext.containsBean("auditLogController")).isTrue();
        assertThat(applicationContext.containsBean("financialStatementController")).isTrue();
        assertThat(applicationContext.containsBean("journalEntryEventSourcingController")).isTrue();

        // Event listener and Projection
        assertThat(applicationContext.containsBean("auditEventListenerAdapter")).isTrue();
        assertThat(applicationContext.containsBean("journalEntryProjectionAdapter")).isTrue();
    }

    @Test
    @DisplayName("アクチュエーターヘルスエンドポイントが応答する")
    void healthEndpointResponds() {
        ResponseEntity<String> response = restTemplate.getForEntity("/actuator/health", String.class);
        assertThat(response.getStatusCode()).isEqualTo(HttpStatus.OK);
    }

    @Test
    @DisplayName("Swagger UI が利用可能")
    void swaggerUiIsAvailable() {
        ResponseEntity<String> response = restTemplate.getForEntity("/swagger-ui/index.html", String.class);
        assertThat(response.getStatusCode()).isEqualTo(HttpStatus.OK);
    }

    @Test
    @DisplayName("OpenAPI ドキュメントが利用可能")
    void openApiDocumentIsAvailable() {
        ResponseEntity<String> response = restTemplate.getForEntity("/v3/api-docs", String.class);
        assertThat(response.getStatusCode()).isEqualTo(HttpStatus.OK);
        assertThat(response.getBody()).contains("openapi");
    }

    @Test
    @DisplayName("データベース接続が正常")
    void databaseConnectionIsHealthy() {
        assertThat(postgres.isRunning()).isTrue();
        assertThat(postgres.isCreated()).isTrue();
    }

    @Test
    @DisplayName("Flyway マイグレーションが正常に実行される")
    void flywayMigrationSucceeds() {
        // Spring Boot が起動時に Flyway を実行するため、
        // コンテキストがロードされていればマイグレーションは成功している
        assertThat(applicationContext.containsBean("flyway")).isTrue();
    }

    @Test
    @DisplayName("勘定科目 API エンドポイントが応答する")
    void accountApiEndpointResponds() {
        ResponseEntity<String> response = restTemplate.getForEntity("/api/v1/accounts", String.class);
        assertThat(response.getStatusCode()).isEqualTo(HttpStatus.OK);
    }

    @Test
    @DisplayName("監査ログ API エンドポイントが応答する")
    void auditLogApiEndpointResponds() {
        ResponseEntity<String> response = restTemplate.getForEntity(
            "/api/v1/audit-logs/entity/Account/test",
            String.class
        );
        assertThat(response.getStatusCode()).isEqualTo(HttpStatus.OK);
    }

    @Test
    @DisplayName("財務諸表 API エンドポイントが応答する")
    void financialStatementApiEndpointResponds() {
        ResponseEntity<String> response = restTemplate.getForEntity(
            "/api/v1/financial-statements/balance-sheet?fiscalYear=2024&accountingPeriod=1",
            String.class
        );
        assertThat(response.getStatusCode()).isEqualTo(HttpStatus.OK);
    }

    @Test
    @DisplayName("イベントソーシング版仕訳 API エンドポイントが利用可能")
    void journalEntryEventSourcingApiIsAvailable() {
        // Swagger UI で API が公開されていることを確認
        ResponseEntity<String> response = restTemplate.getForEntity("/v3/api-docs", String.class);
        assertThat(response.getStatusCode()).isEqualTo(HttpStatus.OK);
        assertThat(response.getBody()).contains("journal-entries-es");
    }
}
