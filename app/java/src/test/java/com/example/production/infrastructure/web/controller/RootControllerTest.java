package com.example.production.infrastructure.web.controller;

import com.example.production.testsetup.BaseIntegrationTest;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.webmvc.test.autoconfigure.AutoConfigureMockMvc;
import org.springframework.test.web.servlet.MockMvc;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * ルート API コントローラのテスト
 */
@AutoConfigureMockMvc
@DisplayName("ルート API")
class RootControllerTest extends BaseIntegrationTest {

    @Autowired
    private MockMvc mockMvc;

    @Nested
    @DisplayName("GET /")
    /* default */ class GetRoot {

        @Test
        @DisplayName("API 情報を取得できる")
        void shouldReturnApiInfo() throws Exception {
            mockMvc.perform(get("/"))
                    .andExpect(status().isOk())
                    .andExpect(jsonPath("$.message").value("生産管理システム API"))
                    .andExpect(jsonPath("$.version").value("1.0.0"))
                    .andExpect(jsonPath("$.endpoints").isArray())
                    .andExpect(jsonPath("$.docs").value("/swagger-ui.html"));
        }
    }

    @Nested
    @DisplayName("GET /health")
    /* default */ class GetHealth {

        @Test
        @DisplayName("ヘルスチェックに成功する")
        void shouldReturnHealthStatus() throws Exception {
            mockMvc.perform(get("/health"))
                    .andExpect(status().isOk())
                    .andExpect(jsonPath("$.status").value("ok"))
                    .andExpect(jsonPath("$.timestamp").exists());
        }
    }

    @Nested
    @DisplayName("GET /swagger-ui.html")
    /* default */ class GetSwaggerUI {

        @Test
        @DisplayName("Swagger UI にアクセスできる")
        void shouldAccessSwaggerUI() throws Exception {
            mockMvc.perform(get("/swagger-ui.html"))
                    .andExpect(status().is3xxRedirection());
        }
    }
}
