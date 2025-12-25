package com.example.production.integration;

import com.example.production.testsetup.TestcontainersConfiguration;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.web.server.LocalServerPort;
import org.springframework.context.annotation.Import;
import org.springframework.http.MediaType;
import org.springframework.web.client.RestClient;

import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * MRP API 統合テスト
 */
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@Import(TestcontainersConfiguration.class)
@DisplayName("MRP API 統合テスト")
class MrpApiIntegrationTest {

    @LocalServerPort
    private int port;

    private RestClient restClient;

    @BeforeEach
    void setUp() {
        restClient = RestClient.create("http://localhost:" + port);
    }

    @Nested
    @DisplayName("POST /api/mrp/execute")
    class ExecuteMrp {

        @Test
        @DisplayName("MRPを実行できる")
        @SuppressWarnings("unchecked")
        void shouldExecuteMrp() {
            String request = """
                {
                    "startDate": "2025-01-01",
                    "endDate": "2025-01-31"
                }
                """;

            Map<String, Object> response = restClient.post()
                    .uri("/api/mrp/execute")
                    .contentType(MediaType.APPLICATION_JSON)
                    .body(request)
                    .retrieve()
                    .body(Map.class);

            assertThat(response).isNotNull();
            assertThat(response.get("executionTime")).isNotNull();
            assertThat(response.get("periodStart")).isEqualTo("2025-01-01");
            assertThat(response.get("periodEnd")).isEqualTo("2025-01-31");
        }
    }

    @Nested
    @DisplayName("GET /api/mrp/results")
    class GetResults {

        @Test
        @DisplayName("MRP実行結果を照会できる")
        @SuppressWarnings("unchecked")
        void shouldGetResults() {
            Map<String, Object> response = restClient.get()
                    .uri("/api/mrp/results")
                    .retrieve()
                    .body(Map.class);

            assertThat(response).isNotNull();
            assertThat(response.get("message")).isNotNull();
        }
    }

    @Nested
    @DisplayName("GET /api/mrp/planned-orders")
    class GetPlannedOrders {

        @Test
        @DisplayName("計画オーダ一覧を取得できる")
        @SuppressWarnings("unchecked")
        void shouldGetPlannedOrders() {
            Map<String, Object> response = restClient.get()
                    .uri("/api/mrp/planned-orders")
                    .retrieve()
                    .body(Map.class);

            assertThat(response).isNotNull();
            assertThat(response.get("message")).isNotNull();
        }
    }
}
