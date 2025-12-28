package com.example.production.infrastructure.web.controller;

import com.example.production.testsetup.TestcontainersConfiguration;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.web.server.LocalServerPort;
import org.springframework.context.annotation.Import;
import org.springframework.web.client.RestClient;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * ホーム画面 Controller テスト
 */
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@Import(TestcontainersConfiguration.class)
@DisplayName("ホーム画面")
class HomeControllerTest {

    @LocalServerPort
    private int port;

    private RestClient restClient;

    @BeforeEach
    void setUp() {
        restClient = RestClient.create("http://localhost:" + port);
    }

    @Test
    @DisplayName("ダッシュボード画面を表示できる")
    void shouldDisplayDashboard() {
        String response = restClient.get()
                .uri("/")
                .retrieve()
                .body(String.class);

        assertThat(response)
                .isNotNull()
                .contains("ダッシュボード")
                .contains("本日")
                .contains("登録品目数");
    }

    @Test
    @DisplayName("ダッシュボードにクイックアクセスが表示される")
    void shouldDisplayQuickAccess() {
        String response = restClient.get()
                .uri("/")
                .retrieve()
                .body(String.class);

        assertThat(response)
                .isNotNull()
                .contains("クイックアクセス")
                .contains("品目登録");
    }

    @Test
    @DisplayName("ダッシュボードにナビゲーションが表示される")
    void shouldDisplayNavigation() {
        String response = restClient.get()
                .uri("/")
                .retrieve()
                .body(String.class);

        assertThat(response)
                .isNotNull()
                .contains("マスタ")
                .contains("計画")
                .contains("購買")
                .contains("製造")
                .contains("在庫");
    }
}
