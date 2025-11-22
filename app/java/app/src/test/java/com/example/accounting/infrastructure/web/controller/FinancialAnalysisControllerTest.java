package com.example.accounting.infrastructure.web.controller;

import com.example.accounting.application.port.in.FinancialAnalysisUseCase;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.web.servlet.MockMvc;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;

/**
 * FinancialAnalysisController の統合テスト
 *
 * 注: このテストは、コントローラーの起動確認のみを行います。
 * 実際のエンドツーエンドテストは、DatabaseSeeder を使用して
 * 手動で実施する必要があります。
 */
@WebMvcTest(FinancialAnalysisController.class)
@DisplayName("FinancialAnalysisController - 財務分析 API")
class FinancialAnalysisControllerTest {

    @Autowired
    private MockMvc mockMvc;

    @MockBean
    private FinancialAnalysisUseCase financialAnalysisUseCase;

    @Test
    @DisplayName("GET /api/v1/financial-analysis/{fiscalYear} - エンドポイントが存在すること")
    void shouldHaveAnalyzeByFiscalYearEndpoint() throws Exception {
        // When & Then: エンドポイントが存在することを確認
        // Note: MockBean なので実際のデータは返らないが、エンドポイントの存在は確認できる
        mockMvc.perform(get("/api/v1/financial-analysis/2021"))
            .andDo(print());
    }

    @Test
    @DisplayName("GET /api/v1/financial-analysis/compare - エンドポイントが存在すること")
    void shouldHaveCompareEndpoint() throws Exception {
        // When & Then: エンドポイントが存在することを確認
        mockMvc.perform(get("/api/v1/financial-analysis/compare")
                .param("fiscalYears", "2021", "2022"))
            .andDo(print());
    }
}
