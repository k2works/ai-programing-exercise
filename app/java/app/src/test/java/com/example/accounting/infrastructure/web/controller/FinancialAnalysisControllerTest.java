package com.example.accounting.infrastructure.web.controller;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.DynamicPropertyRegistry;
import org.springframework.test.context.DynamicPropertySource;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.transaction.annotation.Transactional;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * FinancialAnalysisController の統合テスト
 */
@SpringBootTest
@AutoConfigureMockMvc
@ActiveProfiles("test")
@Transactional
@Testcontainers
@DisplayName("FinancialAnalysisController - 財務分析 API")
class FinancialAnalysisControllerTest {

    @Container
    static PostgreSQLContainer<?> postgres = new PostgreSQLContainer<>("postgres:16-alpine")
        .withDatabaseName("testdb")
        .withUsername("test")
        .withPassword("test");

    @DynamicPropertySource
    static void configureProperties(DynamicPropertyRegistry registry) {
        registry.add("spring.datasource.url", postgres::getJdbcUrl);
        registry.add("spring.datasource.username", postgres::getUsername);
        registry.add("spring.datasource.password", postgres::getPassword);
    }

    @Autowired
    private MockMvc mockMvc;

    @Test
    @DisplayName("GET /api/v1/financial-analysis/{fiscalYear} - データがない場合はエラーを返すこと")
    void shouldReturnErrorWhenNoData() throws Exception {
        // Given: データが存在しない状態

        // When & Then: 財務分析を実行するとエラーが返る
        mockMvc.perform(get("/api/v1/financial-analysis/2021"))
            .andDo(print())
            .andExpect(status().is5xxServerError());
    }

    @Test
    @DisplayName("GET /api/v1/financial-analysis/compare - データがない場合はエラーを返すこと")
    void shouldReturnErrorWhenNoDataForComparison() throws Exception {
        // Given: データが存在しない状態

        // When & Then: 比較分析を実行するとエラーが返る
        mockMvc.perform(get("/api/v1/financial-analysis/compare")
                .param("fiscalYears", "2021", "2022"))
            .andDo(print())
            .andExpect(status().is5xxServerError());
    }
}
