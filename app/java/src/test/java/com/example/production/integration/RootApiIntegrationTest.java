package com.example.production.integration;

import com.example.production.testsetup.TestcontainersConfiguration;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.web.server.LocalServerPort;
import org.springframework.context.annotation.Import;
import org.springframework.http.HttpStatus;
import org.springframework.web.client.RestClient;

import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * ルート API 統合テスト
 */
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@Import(TestcontainersConfiguration.class)
@DisplayName("ルート API 統合テスト")
class RootApiIntegrationTest {

    @LocalServerPort
    private int port;

    private RestClient restClient;

    @BeforeEach
    void setUp() {
        restClient = RestClient.create("http://localhost:" + port);
    }

    @Nested
    @DisplayName("GET /")
    class GetRoot {

        @Test
        @DisplayName("API 情報を取得できる")
        @SuppressWarnings("unchecked")
        void shouldReturnApiInfo() {
            Map<String, Object> response = restClient.get()
                    .uri("/")
                    .retrieve()
                    .body(Map.class);

            assertThat(response).isNotNull()
                    .containsEntry("message", "生産管理システム API")
                    .containsEntry("version", "1.0.0")
                    .containsEntry("docs", "/swagger-ui.html");
            assertThat(response.get("endpoints")).isNotNull();
        }
    }

    @Nested
    @DisplayName("GET /health")
    class GetHealth {

        @Test
        @DisplayName("ヘルスチェックに成功する")
        @SuppressWarnings("unchecked")
        void shouldReturnHealthStatus() {
            Map<String, Object> response = restClient.get()
                    .uri("/health")
                    .retrieve()
                    .body(Map.class);

            assertThat(response).isNotNull()
                    .containsEntry("status", "ok");
            assertThat(response.get("timestamp")).isNotNull();
        }
    }

    @Nested
    @DisplayName("GET /swagger-ui.html")
    class GetSwaggerUI {

        @Test
        @DisplayName("Swagger UI にアクセスできる")
        void shouldAccessSwaggerUI() {
            var responseEntity = restClient.get()
                    .uri("/swagger-ui.html")
                    .retrieve()
                    .toBodilessEntity();

            // リダイレクトまたは成功
            assertThat(responseEntity.getStatusCode())
                    .satisfiesAnyOf(
                            status -> assertThat(status).isEqualTo(HttpStatus.OK),
                            status -> assertThat(status.is3xxRedirection()).isTrue()
                    );
        }
    }
}
