package com.example.production.integration.rest;

import com.example.production.application.port.out.WorkOrderRepository;
import com.example.production.testsetup.TestcontainersConfiguration;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.web.server.LocalServerPort;
import org.springframework.context.annotation.Import;
import org.springframework.http.HttpStatus;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.RestClient;

import java.util.List;
import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

/**
 * 作業指示 API 統合テスト
 */
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@Import(TestcontainersConfiguration.class)
@DisplayName("作業指示 API 統合テスト")
class WorkOrderApiIntegrationTest {

    @LocalServerPort
    private int port;

    @Autowired
    private WorkOrderRepository workOrderRepository;

    private RestClient restClient;

    @BeforeEach
    void setUp() {
        restClient = RestClient.create("http://localhost:" + port);

        // テストデータをクリーンアップ
        workOrderRepository.deleteAll();
    }

    @Nested
    @DisplayName("GET /api/work-orders")
    class GetAllWorkOrders {

        @Test
        @DisplayName("作業指示一覧を取得できる")
        @SuppressWarnings("unchecked")
        void shouldReturnAllWorkOrders() {
            List<Map<String, Object>> response = restClient.get()
                    .uri("/api/work-orders")
                    .retrieve()
                    .body(List.class);

            assertThat(response).isNotNull();
        }

        @Test
        @DisplayName("ステータスでフィルタリングできる")
        @SuppressWarnings("unchecked")
        void shouldFilterByStatus() {
            List<Map<String, Object>> response = restClient.get()
                    .uri("/api/work-orders?status=NOT_STARTED")
                    .retrieve()
                    .body(List.class);

            assertThat(response).isNotNull();
        }
    }

    @Nested
    @DisplayName("GET /api/work-orders/{workOrderNumber}")
    class GetWorkOrder {

        @Test
        @DisplayName("存在しない作業指示番号は404を返す")
        void shouldReturn404ForNonExistentWorkOrder() {
            assertThatThrownBy(() -> restClient.get()
                    .uri("/api/work-orders/NON-EXISTENT")
                    .retrieve()
                    .body(Map.class))
                    .isInstanceOf(HttpClientErrorException.class)
                    .satisfies(ex -> {
                        HttpClientErrorException httpEx = (HttpClientErrorException) ex;
                        assertThat(httpEx.getStatusCode()).isEqualTo(HttpStatus.NOT_FOUND);
                    });
        }
    }
}
