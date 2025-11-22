package com.example.accounting.infrastructure.in.web.controller;

import com.example.accounting.infrastructure.in.web.controller.JournalEntryEventSourcingController.CreateJournalEntryRequest;
import com.example.accounting.infrastructure.in.web.controller.JournalEntryEventSourcingController.ApproveJournalEntryRequest;
import com.example.accounting.infrastructure.in.web.controller.JournalEntryEventSourcingController.DeleteJournalEntryRequest;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.MediaType;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.test.context.DynamicPropertyRegistry;
import org.springframework.test.context.DynamicPropertySource;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * イベントソーシング版仕訳 REST API のテスト
 */
@SpringBootTest
@AutoConfigureMockMvc
@Testcontainers
@DisplayName("JournalEntryEventSourcingController のテスト")
class JournalEntryEventSourcingControllerTest {

    @Container
    static PostgreSQLContainer<?> postgres = new PostgreSQLContainer<>("postgres:15-alpine")
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

    @Autowired
    private ObjectMapper objectMapper;

    @Autowired
    private JdbcTemplate jdbcTemplate;

    @BeforeEach
    void setUp() {
        cleanup();
    }

    @AfterEach
    void tearDown() {
        cleanup();
    }

    private void cleanup() {
        jdbcTemplate.execute("DELETE FROM \"イベントストア\"");
    }

    @Test
    @DisplayName("POST /api/v1/journal-entries-es で仕訳を作成できる")
    void shouldCreateJournalEntry() throws Exception {
        // Given
        CreateJournalEntryRequest request = new CreateJournalEntryRequest();
        request.setEntryDate(LocalDate.of(2024, 1, 15));
        request.setDescription("売上計上");
        request.setUserId("user1");

        CreateJournalEntryRequest.LineItemRequest debitItem =
            new CreateJournalEntryRequest.LineItemRequest();
        debitItem.setAccountCode("1001");
        debitItem.setDebitCredit("DEBIT");
        debitItem.setAmount(new BigDecimal("100000"));

        CreateJournalEntryRequest.LineItemRequest creditItem =
            new CreateJournalEntryRequest.LineItemRequest();
        creditItem.setAccountCode("4001");
        creditItem.setDebitCredit("CREDIT");
        creditItem.setAmount(new BigDecimal("100000"));

        request.setLineItems(List.of(debitItem, creditItem));

        // When & Then
        mockMvc.perform(post("/api/v1/journal-entries-es")
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(request)))
            .andExpect(status().isCreated())
            .andExpect(jsonPath("$.id").isNotEmpty());
    }

    @Test
    @DisplayName("GET /api/v1/journal-entries-es/{id} でイベント再生により仕訳を取得できる")
    void shouldGetJournalEntryByEventReplay() throws Exception {
        // Given: 仕訳を作成
        String journalEntryId = createJournalEntry();

        // When & Then
        mockMvc.perform(get("/api/v1/journal-entries-es/{id}", journalEntryId))
            .andExpect(status().isOk())
            .andExpect(jsonPath("$.id").value(journalEntryId))
            .andExpect(jsonPath("$.entryDate").value("2024-01-15"))
            .andExpect(jsonPath("$.description").value("売上計上"))
            .andExpect(jsonPath("$.status").value("DRAFT"))
            .andExpect(jsonPath("$.deleted").value(false))
            .andExpect(jsonPath("$.lineItems").isArray())
            .andExpect(jsonPath("$.lineItems.length()").value(2));
    }

    @Test
    @DisplayName("POST /api/v1/journal-entries-es/{id}/approve で仕訳を承認できる")
    void shouldApproveJournalEntry() throws Exception {
        // Given
        String journalEntryId = createJournalEntry();

        ApproveJournalEntryRequest request = new ApproveJournalEntryRequest();
        request.setApprovedBy("manager1");
        request.setComment("承認します");

        // When & Then
        mockMvc.perform(post("/api/v1/journal-entries-es/{id}/approve", journalEntryId)
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(request)))
            .andExpect(status().isNoContent());

        // 承認後の状態を確認
        mockMvc.perform(get("/api/v1/journal-entries-es/{id}", journalEntryId))
            .andExpect(status().isOk())
            .andExpect(jsonPath("$.status").value("APPROVED"));
    }

    @Test
    @DisplayName("DELETE /api/v1/journal-entries-es/{id} で仕訳を削除できる")
    void shouldDeleteJournalEntry() throws Exception {
        // Given
        String journalEntryId = createJournalEntry();

        DeleteJournalEntryRequest request = new DeleteJournalEntryRequest();
        request.setReason("誤入力のため");
        request.setUserId("user1");

        // When & Then
        mockMvc.perform(delete("/api/v1/journal-entries-es/{id}", journalEntryId)
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(request)))
            .andExpect(status().isNoContent());

        // 削除後の状態を確認
        mockMvc.perform(get("/api/v1/journal-entries-es/{id}", journalEntryId))
            .andExpect(status().isOk())
            .andExpect(jsonPath("$.deleted").value(true));
    }

    @Test
    @DisplayName("借方と貸方が不均衡な仕訳は作成できない")
    void shouldNotCreateUnbalancedJournalEntry() throws Exception {
        // Given
        CreateJournalEntryRequest request = new CreateJournalEntryRequest();
        request.setEntryDate(LocalDate.of(2024, 1, 15));
        request.setDescription("不均衡な仕訳");
        request.setUserId("user1");

        CreateJournalEntryRequest.LineItemRequest debitItem =
            new CreateJournalEntryRequest.LineItemRequest();
        debitItem.setAccountCode("1001");
        debitItem.setDebitCredit("DEBIT");
        debitItem.setAmount(new BigDecimal("100000"));

        CreateJournalEntryRequest.LineItemRequest creditItem =
            new CreateJournalEntryRequest.LineItemRequest();
        creditItem.setAccountCode("4001");
        creditItem.setDebitCredit("CREDIT");
        creditItem.setAmount(new BigDecimal("50000"));

        request.setLineItems(List.of(debitItem, creditItem));

        // When & Then
        mockMvc.perform(post("/api/v1/journal-entries-es")
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(request)))
            .andExpect(status().isBadRequest());
    }

    @Test
    @DisplayName("存在しない仕訳は取得できない")
    void shouldReturn404WhenJournalEntryNotFound() throws Exception {
        mockMvc.perform(get("/api/v1/journal-entries-es/{id}", "non-existent-id"))
            .andExpect(status().isBadRequest());
    }

    /**
     * テスト用仕訳作成ヘルパー
     */
    private String createJournalEntry() throws Exception {
        CreateJournalEntryRequest request = new CreateJournalEntryRequest();
        request.setEntryDate(LocalDate.of(2024, 1, 15));
        request.setDescription("売上計上");
        request.setUserId("user1");

        CreateJournalEntryRequest.LineItemRequest debitItem =
            new CreateJournalEntryRequest.LineItemRequest();
        debitItem.setAccountCode("1001");
        debitItem.setDebitCredit("DEBIT");
        debitItem.setAmount(new BigDecimal("100000"));

        CreateJournalEntryRequest.LineItemRequest creditItem =
            new CreateJournalEntryRequest.LineItemRequest();
        creditItem.setAccountCode("4001");
        creditItem.setDebitCredit("CREDIT");
        creditItem.setAmount(new BigDecimal("100000"));

        request.setLineItems(List.of(debitItem, creditItem));

        MvcResult result = mockMvc.perform(post("/api/v1/journal-entries-es")
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(request)))
            .andExpect(status().isCreated())
            .andReturn();

        String responseBody = result.getResponse().getContentAsString();
        return objectMapper.readTree(responseBody).get("id").asText();
    }
}
