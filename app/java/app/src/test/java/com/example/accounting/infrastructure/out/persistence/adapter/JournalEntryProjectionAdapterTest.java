package com.example.accounting.infrastructure.out.persistence.adapter;

import com.example.accounting.application.service.finacial.JournalEntryEventSourcingService;
import com.example.accounting.application.service.finacial.JournalEntryQueryService;
import com.example.accounting.infrastructure.out.persistence.dao.JournalEntryReadModel;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.test.context.DynamicPropertyRegistry;
import org.springframework.test.context.DynamicPropertySource;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.awaitility.Awaitility.await;
import static java.util.concurrent.TimeUnit.SECONDS;

/**
 * JournalEntryProjectionAdapter の統合テスト（CQRS パターン）
 */
@SpringBootTest
@Testcontainers
@DisplayName("JournalEntryProjectionAdapter の統合テスト")
class JournalEntryProjectionAdapterTest {

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
    private JournalEntryEventSourcingService commandService;

    @Autowired
    private JournalEntryQueryService queryService;

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
        jdbcTemplate.execute("DELETE FROM journal_entry_line_read_model");
        jdbcTemplate.execute("DELETE FROM journal_entry_read_model");
        jdbcTemplate.execute("DELETE FROM \"イベントストア\"");
    }

    @Test
    @DisplayName("仕訳作成イベントが Read Model に投影される")
    void shouldProjectJournalEntryCreatedEvent() {
        // Given
        LocalDate entryDate = LocalDate.of(2024, 1, 15);
        String description = "売上計上";
        List<JournalEntryEventSourcingService.LineItemDto> lineItems = List.of(
            createLineItemDto("1001", "DEBIT", new BigDecimal("100000")),
            createLineItemDto("4001", "CREDIT", new BigDecimal("100000"))
        );

        // When: Command 実行
        String journalEntryId = commandService.createJournalEntry(
            entryDate, description, lineItems, "user1"
        );

        // Then: Read Model が更新されている（非同期処理のため await を使用）
        await().atMost(5, SECONDS).untilAsserted(() -> {
            JournalEntryReadModel readModel = queryService.findById(journalEntryId);
            assertThat(readModel).isNotNull();
            assertThat(readModel.getEntryDate()).isEqualTo(entryDate);
            assertThat(readModel.getDescription()).isEqualTo(description);
            assertThat(readModel.getStatus()).isEqualTo("DRAFT");
            assertThat(readModel.getDeleted()).isFalse();
            assertThat(readModel.getLineItems()).hasSize(2);
        });
    }

    @Test
    @DisplayName("仕訳承認イベントが Read Model に投影される")
    void shouldProjectJournalEntryApprovedEvent() {
        // Given
        String journalEntryId = commandService.createJournalEntry(
            LocalDate.of(2024, 1, 15),
            "売上計上",
            List.of(
                createLineItemDto("1001", "DEBIT", new BigDecimal("100000")),
                createLineItemDto("4001", "CREDIT", new BigDecimal("100000"))
            ),
            "user1"
        );

        // Wait for creation projection
        await().atMost(5, SECONDS).untilAsserted(() -> {
            JournalEntryReadModel readModel = queryService.findById(journalEntryId);
            assertThat(readModel.getStatus()).isEqualTo("DRAFT");
        });

        // When: 承認
        commandService.approveJournalEntry(journalEntryId, "manager1", "承認します");

        // Then: Read Model が更新されている
        await().atMost(5, SECONDS).untilAsserted(() -> {
            JournalEntryReadModel readModel = queryService.findById(journalEntryId);
            assertThat(readModel.getStatus()).isEqualTo("APPROVED");
            assertThat(readModel.getApprovedBy()).isEqualTo("manager1");
            assertThat(readModel.getApprovalComment()).isEqualTo("承認します");
        });
    }

    @Test
    @DisplayName("仕訳削除イベントが Read Model に投影される")
    void shouldProjectJournalEntryDeletedEvent() {
        // Given
        String journalEntryId = commandService.createJournalEntry(
            LocalDate.of(2024, 1, 15),
            "売上計上",
            List.of(
                createLineItemDto("1001", "DEBIT", new BigDecimal("100000")),
                createLineItemDto("4001", "CREDIT", new BigDecimal("100000"))
            ),
            "user1"
        );

        // Wait for creation projection
        await().atMost(5, SECONDS).untilAsserted(() -> {
            JournalEntryReadModel readModel = queryService.findById(journalEntryId);
            assertThat(readModel.getDeleted()).isFalse();
        });

        // When: 削除
        commandService.deleteJournalEntry(journalEntryId, "誤入力のため", "user1");

        // Then: Read Model が更新されている
        await().atMost(5, SECONDS).untilAsserted(() -> {
            JournalEntryReadModel readModel = queryService.findById(journalEntryId);
            assertThat(readModel.getDeleted()).isTrue();
        });
    }

    @Test
    @DisplayName("Query Service で仕訳を検索できる")
    void shouldQueryJournalEntries() {
        // Given: 複数の仕訳を作成
        commandService.createJournalEntry(
            LocalDate.of(2024, 1, 15),
            "売上計上1",
            List.of(
                createLineItemDto("1001", "DEBIT", new BigDecimal("100000")),
                createLineItemDto("4001", "CREDIT", new BigDecimal("100000"))
            ),
            "user1"
        );

        String journalEntryId2 = commandService.createJournalEntry(
            LocalDate.of(2024, 1, 16),
            "売上計上2",
            List.of(
                createLineItemDto("1001", "DEBIT", new BigDecimal("200000")),
                createLineItemDto("4001", "CREDIT", new BigDecimal("200000"))
            ),
            "user1"
        );

        // 承認
        commandService.approveJournalEntry(journalEntryId2, "manager1", "承認");

        // Wait for projections
        await().atMost(5, SECONDS).untilAsserted(() -> {
            List<JournalEntryReadModel> all = queryService.findAll();
            assertThat(all).hasSize(2);
        });

        // When & Then: ステータスで検索
        await().atMost(5, SECONDS).untilAsserted(() -> {
            List<JournalEntryReadModel> draftEntries = queryService.findByStatus("DRAFT");
            assertThat(draftEntries).hasSize(1);
            assertThat(draftEntries.get(0).getDescription()).isEqualTo("売上計上1");

            List<JournalEntryReadModel> approvedEntries = queryService.findByStatus("APPROVED");
            assertThat(approvedEntries).hasSize(1);
            assertThat(approvedEntries.get(0).getDescription()).isEqualTo("売上計上2");
        });

        // When & Then: 起票日で検索
        List<JournalEntryReadModel> entriesOnDate = queryService.findByEntryDate(LocalDate.of(2024, 1, 15));
        assertThat(entriesOnDate).hasSize(1);
        assertThat(entriesOnDate.get(0).getDescription()).isEqualTo("売上計上1");
    }

    private JournalEntryEventSourcingService.LineItemDto createLineItemDto(
            String accountCode, String debitCredit, BigDecimal amount) {
        JournalEntryEventSourcingService.LineItemDto dto =
            new JournalEntryEventSourcingService.LineItemDto();
        dto.setAccountCode(accountCode);
        dto.setDebitCredit(debitCredit);
        dto.setAmount(amount);
        return dto;
    }
}
