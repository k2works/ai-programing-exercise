package com.example.accounting.application.service;

import com.example.accounting.application.service.finacial.JournalEntryEventSourcingService;
import com.example.accounting.domain.aggregate.JournalEntryAggregate;
import com.example.accounting.domain.aggregate.JournalEntryStatus;
import com.example.accounting.domain.event.DomainEvent;
import com.example.accounting.application.exception.ConcurrentModificationException;
import com.example.accounting.application.port.out.EventStoreRepository;
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
import static org.assertj.core.api.Assertions.assertThatThrownBy;

/**
 * イベントソーシング版仕訳サービスの統合テスト
 */
@SpringBootTest
@Testcontainers
@DisplayName("JournalEntryEventSourcingService の統合テスト")
class JournalEntryEventSourcingServiceTest {

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
    private JournalEntryEventSourcingService service;

    @Autowired
    private EventStoreRepository eventStoreRepository;

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
    @DisplayName("仕訳を作成できる")
    void shouldCreateJournalEntry() {
        // Given
        LocalDate entryDate = LocalDate.of(2024, 1, 15);
        String description = "売上計上";
        List<JournalEntryEventSourcingService.LineItemDto> lineItems = List.of(
            createLineItemDto("1001", "DEBIT", new BigDecimal("100000")),
            createLineItemDto("4001", "CREDIT", new BigDecimal("100000"))
        );

        // When
        String journalEntryId = service.createJournalEntry(entryDate, description, lineItems, "user1");

        // Then
        assertThat(journalEntryId).isNotNull();

        // イベントストアにイベントが保存されている
        List<DomainEvent> events = eventStoreRepository.getEvents(journalEntryId);
        assertThat(events).hasSize(1);
        assertThat(events.get(0).getEventType()).isEqualTo("JournalEntryCreatedEvent");

        // イベント再生で Aggregate を復元
        JournalEntryAggregate aggregate = JournalEntryAggregate.replay(events);
        assertThat(aggregate.getId()).isEqualTo(journalEntryId);
        assertThat(aggregate.getEntryDate()).isEqualTo(entryDate);
        assertThat(aggregate.getDescription()).isEqualTo(description);
        assertThat(aggregate.getStatus()).isEqualTo(JournalEntryStatus.DRAFT);
        assertThat(aggregate.getLineItems()).hasSize(2);
    }

    @Test
    @DisplayName("仕訳を承認できる")
    void shouldApproveJournalEntry() {
        // Given
        String journalEntryId = service.createJournalEntry(
            LocalDate.of(2024, 1, 15),
            "売上計上",
            List.of(
                createLineItemDto("1001", "DEBIT", new BigDecimal("100000")),
                createLineItemDto("4001", "CREDIT", new BigDecimal("100000"))
            ),
            "user1"
        );

        // When
        service.approveJournalEntry(journalEntryId, "manager1", "承認します");

        // Then
        List<DomainEvent> events = eventStoreRepository.getEvents(journalEntryId);
        assertThat(events).hasSize(2); // Created + Approved

        JournalEntryAggregate aggregate = JournalEntryAggregate.replay(events);
        assertThat(aggregate.getStatus()).isEqualTo(JournalEntryStatus.APPROVED);
    }

    @Test
    @DisplayName("仕訳を削除できる")
    void shouldDeleteJournalEntry() {
        // Given
        String journalEntryId = service.createJournalEntry(
            LocalDate.of(2024, 1, 15),
            "売上計上",
            List.of(
                createLineItemDto("1001", "DEBIT", new BigDecimal("100000")),
                createLineItemDto("4001", "CREDIT", new BigDecimal("100000"))
            ),
            "user1"
        );

        // When
        service.deleteJournalEntry(journalEntryId, "誤入力のため", "user1");

        // Then
        List<DomainEvent> events = eventStoreRepository.getEvents(journalEntryId);
        assertThat(events).hasSize(2); // Created + Deleted

        JournalEntryAggregate aggregate = JournalEntryAggregate.replay(events);
        assertThat(aggregate.isDeleted()).isTrue();
    }

    @Test
    @DisplayName("削除済みの仕訳は承認できない")
    void shouldNotApproveDeletedJournalEntry() {
        // Given
        String journalEntryId = service.createJournalEntry(
            LocalDate.of(2024, 1, 15),
            "売上計上",
            List.of(
                createLineItemDto("1001", "DEBIT", new BigDecimal("100000")),
                createLineItemDto("4001", "CREDIT", new BigDecimal("100000"))
            ),
            "user1"
        );
        service.deleteJournalEntry(journalEntryId, "誤入力のため", "user1");

        // When & Then
        assertThatThrownBy(() -> service.approveJournalEntry(journalEntryId, "manager1", "承認します"))
            .isInstanceOf(IllegalStateException.class)
            .hasMessageContaining("削除済みの仕訳は承認できません");
    }

    @Test
    @DisplayName("承認済みの仕訳は再承認できない")
    void shouldNotApproveAlreadyApprovedJournalEntry() {
        // Given
        String journalEntryId = service.createJournalEntry(
            LocalDate.of(2024, 1, 15),
            "売上計上",
            List.of(
                createLineItemDto("1001", "DEBIT", new BigDecimal("100000")),
                createLineItemDto("4001", "CREDIT", new BigDecimal("100000"))
            ),
            "user1"
        );
        service.approveJournalEntry(journalEntryId, "manager1", "承認します");

        // When & Then
        assertThatThrownBy(() -> service.approveJournalEntry(journalEntryId, "manager2", "再承認"))
            .isInstanceOf(IllegalStateException.class)
            .hasMessageContaining("すでに承認済みです");
    }

    @Test
    @DisplayName("借方と貸方の合計が一致しない仕訳は作成できない")
    void shouldNotCreateUnbalancedJournalEntry() {
        // Given
        List<JournalEntryEventSourcingService.LineItemDto> lineItems = List.of(
            createLineItemDto("1001", "DEBIT", new BigDecimal("100000")),
            createLineItemDto("4001", "CREDIT", new BigDecimal("50000"))
        );

        // When & Then
        assertThatThrownBy(() -> service.createJournalEntry(
            LocalDate.of(2024, 1, 15),
            "不均衡な仕訳",
            lineItems,
            "user1"
        ))
            .isInstanceOf(IllegalArgumentException.class)
            .hasMessageContaining("借方合計と貸方合計が一致しません");
    }

    @Test
    @DisplayName("イベント再生でタイムトラベルができる")
    void shouldReplayEventsForTimeTravel() {
        // Given
        String journalEntryId = service.createJournalEntry(
            LocalDate.of(2024, 1, 15),
            "売上計上",
            List.of(
                createLineItemDto("1001", "DEBIT", new BigDecimal("100000")),
                createLineItemDto("4001", "CREDIT", new BigDecimal("100000"))
            ),
            "user1"
        );

        // 作成直後の状態
        List<DomainEvent> eventsAfterCreate = eventStoreRepository.getEvents(journalEntryId);
        JournalEntryAggregate aggregateAfterCreate = JournalEntryAggregate.replay(eventsAfterCreate);
        assertThat(aggregateAfterCreate.getStatus()).isEqualTo(JournalEntryStatus.DRAFT);

        // 承認
        service.approveJournalEntry(journalEntryId, "manager1", "承認します");

        // 承認後の状態
        List<DomainEvent> eventsAfterApprove = eventStoreRepository.getEvents(journalEntryId);
        JournalEntryAggregate aggregateAfterApprove = JournalEntryAggregate.replay(eventsAfterApprove);
        assertThat(aggregateAfterApprove.getStatus()).isEqualTo(JournalEntryStatus.APPROVED);

        // タイムトラベル: 作成直後の状態を再現
        JournalEntryAggregate pastAggregate = JournalEntryAggregate.replay(
            eventsAfterApprove.subList(0, 1)
        );
        assertThat(pastAggregate.getStatus()).isEqualTo(JournalEntryStatus.DRAFT);
    }

    @Test
    @DisplayName("楽観的ロックが機能する")
    void shouldDetectConcurrentModification() {
        // Given
        String journalEntryId = service.createJournalEntry(
            LocalDate.of(2024, 1, 15),
            "売上計上",
            List.of(
                createLineItemDto("1001", "DEBIT", new BigDecimal("100000")),
                createLineItemDto("4001", "CREDIT", new BigDecimal("100000"))
            ),
            "user1"
        );

        // 2つのトランザクションで同じ Aggregate を取得
        List<DomainEvent> events1 = eventStoreRepository.getEvents(journalEntryId);
        JournalEntryAggregate aggregate1 = JournalEntryAggregate.replay(events1);
        int version1 = aggregate1.getVersion();

        List<DomainEvent> events2 = eventStoreRepository.getEvents(journalEntryId);
        JournalEntryAggregate aggregate2 = JournalEntryAggregate.replay(events2);
        int version2 = aggregate2.getVersion();

        // トランザクション1が先に承認
        aggregate1.approve("manager1", "承認します");
        eventStoreRepository.save(journalEntryId, aggregate1.getUncommittedEvents(), version1);

        // トランザクション2も承認しようとするが、バージョンが古いので失敗
        aggregate2.approve("manager2", "再承認");
        assertThatThrownBy(() ->
            eventStoreRepository.save(journalEntryId, aggregate2.getUncommittedEvents(), version2)
        ).isInstanceOf(ConcurrentModificationException.class);
    }

    private JournalEntryEventSourcingService.LineItemDto createLineItemDto(
            String accountCode, String debitCredit, BigDecimal amount) {
        JournalEntryEventSourcingService.LineItemDto dto = new JournalEntryEventSourcingService.LineItemDto();
        dto.setAccountCode(accountCode);
        dto.setDebitCredit(debitCredit);
        dto.setAmount(amount);
        return dto;
    }
}
