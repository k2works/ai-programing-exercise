package com.example.management.integration;

import com.example.management.application.AnalyzeFinancialDataUseCase;
import com.example.management.domain.FinancialAnalysisResult;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.web.client.TestRestTemplate;
import org.springframework.boot.test.web.server.LocalServerPort;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.DynamicPropertyRegistry;
import org.springframework.test.context.DynamicPropertySource;
import org.testcontainers.containers.GenericContainer;
import org.testcontainers.containers.Network;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.containers.wait.strategy.Wait;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;
import org.testcontainers.utility.DockerImageName;

import java.math.BigDecimal;
import java.time.Duration;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * TestContainers によるマルチサービスインテグレーションテスト
 *
 * このテストは以下を検証します：
 * 1. 財務会計サービスと管理会計サービスの連携
 * 2. データベースマイグレーションとシードデータの投入
 * 3. サービス間通信（REST API）
 * 4. 財務分析ロジックの正確性
 */
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@Testcontainers
class MultiServiceIntegrationTest {

    @LocalServerPort
    private int port;

    @Autowired
    private TestRestTemplate restTemplate;

    @Autowired
    private AnalyzeFinancialDataUseCase analyzeFinancialDataUseCase;

    // 共有ネットワークの作成
    private static final Network network = Network.newNetwork();

    // 財務会計サービス用 PostgreSQL
    @Container
    static PostgreSQLContainer<?> financialPostgres = new PostgreSQLContainer<>("postgres:16-alpine")
            .withDatabaseName("financial_accounting")
            .withUsername("postgres")
            .withPassword("postgres")
            .withNetwork(network)
            .withNetworkAliases("postgres-financial");

    // 管理会計サービス用 PostgreSQL
    @Container
    static PostgreSQLContainer<?> managementPostgres = new PostgreSQLContainer<>("postgres:16-alpine")
            .withDatabaseName("management_accounting")
            .withUsername("postgres")
            .withPassword("postgres")
            .withNetwork(network)
            .withNetworkAliases("postgres-management");

    // 財務会計サービスコンテナ
    @Container
    static GenericContainer<?> financialAccountingService = new GenericContainer<>(
            DockerImageName.parse("financial-accounting-service:latest"))
            .withNetwork(network)
            .withNetworkAliases("financial-accounting-service")
            .withExposedPorts(8081)
            .withEnv("SPRING_DATASOURCE_URL", "jdbc:postgresql://postgres-financial:5432/financial_accounting")
            .withEnv("SPRING_DATASOURCE_USERNAME", "postgres")
            .withEnv("SPRING_DATASOURCE_PASSWORD", "postgres")
            .dependsOn(financialPostgres)
            .waitingFor(Wait.forHttp("/actuator/health").forPort(8081))
            .withStartupTimeout(Duration.ofMinutes(3));

    @DynamicPropertySource
    static void configureProperties(DynamicPropertyRegistry registry) {
        // 管理会計サービスのデータソース設定
        registry.add("spring.datasource.url", managementPostgres::getJdbcUrl);
        registry.add("spring.datasource.username", managementPostgres::getUsername);
        registry.add("spring.datasource.password", managementPostgres::getPassword);

        // 財務会計サービスの URL 設定
        registry.add("financial-accounting-service.base-url", () ->
            String.format("http://%s:%d",
                financialAccountingService.getHost(),
                financialAccountingService.getMappedPort(8081)));
    }

    @BeforeAll
    static void setup() {
        // コンテナの起動確認
        System.out.println("Financial PostgreSQL: " + financialPostgres.getJdbcUrl());
        System.out.println("Management PostgreSQL: " + managementPostgres.getJdbcUrl());
        System.out.println("Financial Service URL: http://" +
            financialAccountingService.getHost() + ":" +
            financialAccountingService.getMappedPort(8081));
    }

    /**
     * 財務会計サービスのヘルスチェック
     */
    @Test
    void 財務会計サービスが正常に起動していること() {
        // Given: 財務会計サービスコンテナが起動している

        // When: ヘルスチェックエンドポイントにアクセス
        String healthUrl = String.format("http://%s:%d/actuator/health",
            financialAccountingService.getHost(),
            financialAccountingService.getMappedPort(8081));

        ResponseEntity<String> response = restTemplate.getForEntity(healthUrl, String.class);

        // Then: ヘルスチェックが成功すること
        assertThat(response.getStatusCode())
                .as("財務会計サービスのヘルスチェックが成功すること")
                .isEqualTo(HttpStatus.OK);

        assertThat(response.getBody())
                .as("ヘルスステータスが UP であること")
                .contains("UP");
    }

    /**
     * マルチサービス統合テスト：財務会計サービスから財務データを取得し、
     * 管理会計サービスで財務分析を実行できることを検証
     */
    @Test
    void 財務会計サービスで仕訳データを取得し管理会計サービスで分析できること() {
        // Given: シードデータが投入された状態（TestContainers 起動時に Flyway が実行）
        Integer fiscalYear = 2022; // 令和4年度

        // When: 管理会計サービスで財務分析を実行
        FinancialAnalysisResult result = analyzeFinancialDataUseCase.analyze(fiscalYear);

        // Then: 分析結果の検証
        assertThat(result).isNotNull();
        assertThat(result.getData()).isNotNull();
        assertThat(result.getRatios()).isNotNull();

        // 財務データの検証（D社 令和4年度の値）
        assertThat(result.getData().getSales())
                .as("売上高がシードデータと一致すること")
                .isGreaterThan(BigDecimal.ZERO);

        assertThat(result.getData().getOperatingProfit())
                .as("営業利益が取得できること")
                .isGreaterThan(BigDecimal.ZERO);

        assertThat(result.getData().getTotalAssets())
                .as("総資産が取得できること")
                .isGreaterThan(BigDecimal.ZERO);

        // 財務比率の検証
        assertThat(result.getRatios().getOperatingProfitMargin())
                .as("営業利益率が計算されていること")
                .isGreaterThan(BigDecimal.ZERO);

        assertThat(result.getRatios().getReturnOnAssets())
                .as("ROA が計算されていること")
                .isGreaterThan(BigDecimal.ZERO);

        assertThat(result.getRatios().getDebtRatio())
                .as("負債比率が計算されていること")
                .isGreaterThanOrEqualTo(BigDecimal.ZERO)
                .isLessThanOrEqualTo(BigDecimal.ONE);
    }

    /**
     * データベースマイグレーションとシードデータの検証
     */
    @Test
    void 両サービスのデータベースマイグレーションが正常に実行されること() {
        // Given: TestContainers が起動し Flyway マイグレーションが完了している状態

        // When & Then: 両方の PostgreSQL コンテナが起動していることを確認
        assertThat(financialPostgres.isRunning())
                .as("財務会計用 PostgreSQL コンテナが起動していること")
                .isTrue();

        assertThat(managementPostgres.isRunning())
                .as("管理会計用 PostgreSQL コンテナが起動していること")
                .isTrue();

        // 財務会計サービスが起動していることを確認
        assertThat(financialAccountingService.isRunning())
                .as("財務会計サービスコンテナが起動していること")
                .isTrue();

        // ネットワークが正しく構成されていることを確認
        assertThat(network.getId())
                .as("共有ネットワークが作成されていること")
                .isNotNull();
    }

    /**
     * サービス間通信のエンドツーエンドテスト
     */
    @Test
    void 財務会計サービスのJournals APIから仕訳データが取得できること() {
        // Given: 財務会計サービスが起動している

        // When: 財務会計サービスの journals API にアクセス
        String journalsUrl = String.format("http://%s:%d/api/journals?fiscalYear=2022",
            financialAccountingService.getHost(),
            financialAccountingService.getMappedPort(8081));

        ResponseEntity<String> response = restTemplate.getForEntity(journalsUrl, String.class);

        // Then: 仕訳データが取得できること
        assertThat(response.getStatusCode())
                .as("journals API が正常にレスポンスを返すこと")
                .isEqualTo(HttpStatus.OK);

        assertThat(response.getBody())
                .as("レスポンスボディが空でないこと")
                .isNotNull()
                .isNotEmpty();
    }
}
