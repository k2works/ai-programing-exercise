package com.example.production.integration;

import com.example.production.testsetup.TestcontainersConfiguration;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.web.server.LocalServerPort;
import org.springframework.context.annotation.Import;
import org.springframework.web.client.RestClient;

import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * API ドキュメント統合テスト
 */
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@Import(TestcontainersConfiguration.class)
@DisplayName("API ドキュメント統合テスト")
class ApiDocumentationIntegrationTest {

    @LocalServerPort
    private int port;

    private RestClient restClient;

    @BeforeEach
    void setUp() {
        restClient = RestClient.create("http://localhost:" + port);
    }

    @Nested
    @DisplayName("GET /api")
    class RootApi {

        @Test
        @DisplayName("API 情報を取得できる")
        @SuppressWarnings("unchecked")
        void shouldReturnApiInfo() {
            Map<String, Object> response = restClient.get()
                    .uri("/api")
                    .retrieve()
                    .body(Map.class);

            assertThat(response).isNotNull();
            assertThat(response.get("message")).isEqualTo("生産管理システム API");
            assertThat(response.get("version")).isEqualTo("1.0.0");
            assertThat(response.get("docs")).isEqualTo("/swagger-ui.html");
        }
    }

    @Nested
    @DisplayName("GET /health")
    class HealthCheck {

        @Test
        @DisplayName("ヘルスチェックが成功する")
        @SuppressWarnings("unchecked")
        void shouldReturnHealthStatus() {
            Map<String, Object> response = restClient.get()
                    .uri("/health")
                    .retrieve()
                    .body(Map.class);

            assertThat(response).isNotNull();
            assertThat(response.get("status")).isEqualTo("ok");
            assertThat(response.get("timestamp")).isNotNull();
        }
    }

    @Nested
    @DisplayName("GET /v3/api-docs")
    class OpenApiDocs {

        @Test
        @DisplayName("OpenAPI ドキュメントを取得できる")
        @SuppressWarnings("unchecked")
        void shouldReturnOpenApiDocs() {
            Map<String, Object> response = restClient.get()
                    .uri("/v3/api-docs")
                    .retrieve()
                    .body(Map.class);

            assertThat(response).isNotNull();
            assertThat(response.get("openapi")).isNotNull();

            Map<String, Object> info = (Map<String, Object>) response.get("info");
            assertThat(info).isNotNull();
            assertThat(info.get("title")).isEqualTo("生産管理システム API");
            assertThat(info.get("version")).isEqualTo("1.0.0");
        }
    }
}
