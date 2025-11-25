package com.example.accounting.integration;

import com.example.accounting.application.service.finacial.JournalEntryEventSourcingService;
import com.example.accounting.domain.event.JournalEntryCreatedEvent;
import org.awaitility.Awaitility;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.amqp.rabbit.annotation.RabbitListener;
import org.springframework.amqp.rabbit.core.RabbitAdmin;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.context.TestConfiguration;
import org.springframework.context.annotation.Bean;
import org.springframework.stereotype.Component;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.DynamicPropertyRegistry;
import org.springframework.test.context.DynamicPropertySource;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.containers.RabbitMQContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.TimeUnit;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * イベント駆動アーキテクチャの統合テスト
 *
 * TestContainers で RabbitMQ と PostgreSQL を起動し、
 * 仕訳イベントがメッセージブローカー経由で配信されることを検証します。
 */
@SpringBootTest
@Testcontainers
@ActiveProfiles("test")
class EventDrivenArchitectureIntegrationTest {

    private static final Logger logger = LoggerFactory.getLogger(EventDrivenArchitectureIntegrationTest.class);

    @Container
    static PostgreSQLContainer<?> postgres = new PostgreSQLContainer<>("postgres:16-alpine")
        .withDatabaseName("accounting_test")
        .withUsername("test")
        .withPassword("test");

    @Container
    static RabbitMQContainer rabbitmq = new RabbitMQContainer("rabbitmq:3-management-alpine")
        .withExposedPorts(5672, 15672);

    @DynamicPropertySource
    static void configureProperties(DynamicPropertyRegistry registry) {
        // PostgreSQL
        registry.add("spring.datasource.url", postgres::getJdbcUrl);
        registry.add("spring.datasource.username", postgres::getUsername);
        registry.add("spring.datasource.password", postgres::getPassword);

        // RabbitMQ
        registry.add("spring.rabbitmq.host", rabbitmq::getHost);
        registry.add("spring.rabbitmq.port", rabbitmq::getAmqpPort);
        registry.add("spring.rabbitmq.username", () -> "guest");
        registry.add("spring.rabbitmq.password", () -> "guest");

        // RabbitMQ 有効化
        registry.add("rabbitmq.enabled", () -> "true");
    }

    @Autowired
    private JournalEntryEventSourcingService journalEntryService;

    @Autowired
    private TestEventCollector eventCollector;

    @Autowired
    private RabbitAdmin rabbitAdmin;

    @BeforeEach
    void setUp() {
        eventCollector.clear();
        logger.info("=== テスト開始 ===");
        logger.info("RabbitMQ URL: amqp://{}:{}", rabbitmq.getHost(), rabbitmq.getAmqpPort());
        logger.info("PostgreSQL URL: {}", postgres.getJdbcUrl());
    }

    /**
     * 仕訳作成イベントが RabbitMQ 経由で配信されることを検証
     */
    @Test
    void 仕訳作成イベントがRabbitMQ経由で配信されること() {
        // Given: 仕訳明細を準備
        List<JournalEntryEventSourcingService.LineItemDto> lineItems = new ArrayList<>();

        var debitItem = new JournalEntryEventSourcingService.LineItemDto();
        debitItem.setAccountCode("1110");  // 現金預金
        debitItem.setDebitCredit("DEBIT");
        debitItem.setAmount(new BigDecimal("100000"));
        lineItems.add(debitItem);

        var creditItem = new JournalEntryEventSourcingService.LineItemDto();
        creditItem.setAccountCode("5010");  // 売上
        creditItem.setDebitCredit("CREDIT");
        creditItem.setAmount(new BigDecimal("100000"));
        lineItems.add(creditItem);

        // When: 仕訳を作成
        String journalEntryId = journalEntryService.createJournalEntry(
            LocalDate.now(),
            "売上計上",
            lineItems,
            "test-user"
        );

        logger.info("仕訳作成完了: journalEntryId={}", journalEntryId);

        // Then: イベントが RabbitMQ 経由で配信されることを確認
        Awaitility.await()
            .atMost(10, TimeUnit.SECONDS)
            .pollInterval(500, TimeUnit.MILLISECONDS)
            .untilAsserted(() -> {
                assertThat(eventCollector.getReceivedEvents())
                    .as("仕訳作成イベントが受信されていること")
                    .isNotEmpty();

                JournalEntryCreatedEvent event = eventCollector.getReceivedEvents().get(0);
                assertThat(event.getJournalEntryId())
                    .as("受信したイベントの仕訳IDが一致すること")
                    .isEqualTo(journalEntryId);

                assertThat(event.getDescription())
                    .as("摘要が一致すること")
                    .isEqualTo("売上計上");

                assertThat(event.getUserId())
                    .as("ユーザーIDが一致すること")
                    .isEqualTo("test-user");

                assertThat(event.getLineItems())
                    .as("明細行が2件含まれること")
                    .hasSize(2);
            });

        logger.info("イベント受信検証完了");
    }

    /**
     * 仕訳承認イベントが RabbitMQ 経由で配信されることを検証
     */
    @Test
    void 仕訳承認イベントがRabbitMQ経由で配信されること() {
        // Given: 仕訳を作成
        List<JournalEntryEventSourcingService.LineItemDto> lineItems = new ArrayList<>();

        var debitItem = new JournalEntryEventSourcingService.LineItemDto();
        debitItem.setAccountCode("1110");
        debitItem.setDebitCredit("DEBIT");
        debitItem.setAmount(new BigDecimal("50000"));
        lineItems.add(debitItem);

        var creditItem = new JournalEntryEventSourcingService.LineItemDto();
        creditItem.setAccountCode("5010");
        creditItem.setDebitCredit("CREDIT");
        creditItem.setAmount(new BigDecimal("50000"));
        lineItems.add(creditItem);

        String journalEntryId = journalEntryService.createJournalEntry(
            LocalDate.now(),
            "テスト仕訳",
            lineItems,
            "test-user"
        );

        // 作成イベントを待つ
        Awaitility.await()
            .atMost(5, TimeUnit.SECONDS)
            .until(() -> !eventCollector.getReceivedEvents().isEmpty());

        eventCollector.clear();

        // When: 仕訳を承認
        journalEntryService.approveJournalEntry(
            journalEntryId,
            "approver-user",
            "承認しました"
        );

        logger.info("仕訳承認完了: journalEntryId={}", journalEntryId);

        // Then: 承認イベントが配信されることを確認
        Awaitility.await()
            .atMost(10, TimeUnit.SECONDS)
            .pollInterval(500, TimeUnit.MILLISECONDS)
            .untilAsserted(() -> {
                assertThat(eventCollector.getApprovedEvents())
                    .as("仕訳承認イベントが受信されていること")
                    .isNotEmpty();
            });

        logger.info("承認イベント受信検証完了");
    }

    /**
     * テスト用イベントコレクター
     *
     * @RabbitListener でイベントを受信し、テストで検証できるようにする
     */
    @Component
    @RabbitListener(queues = "test-event-queue")
    static class TestEventCollector {
        private static final String TEST_QUEUE = "test-event-queue";

        private final List<JournalEntryCreatedEvent> receivedEvents = new ArrayList<>();
        private final List<Object> approvedEvents = new ArrayList<>();
        private final Logger logger = LoggerFactory.getLogger(TestEventCollector.class);

        @org.springframework.amqp.rabbit.annotation.RabbitHandler
        public void handleEvent(JournalEntryCreatedEvent event) {
            logger.info("【テスト】仕訳作成イベント受信: journalEntryId={}", event.getJournalEntryId());
            receivedEvents.add(event);
        }

        @org.springframework.amqp.rabbit.annotation.RabbitHandler
        public void handleEvent(com.example.accounting.domain.event.JournalEntryApprovedEvent event) {
            logger.info("【テスト】仕訳承認イベント受信: journalEntryId={}", event.getJournalEntryId());
            approvedEvents.add(event);
        }

        @org.springframework.amqp.rabbit.annotation.RabbitHandler
        public void handleEvent(com.example.accounting.domain.event.JournalEntryDeletedEvent event) {
            logger.info("【テスト】仕訳削除イベント受信: journalEntryId={}", event.getJournalEntryId());
            approvedEvents.add(event);
        }

        public List<JournalEntryCreatedEvent> getReceivedEvents() {
            return new ArrayList<>(receivedEvents);
        }

        public List<Object> getApprovedEvents() {
            return new ArrayList<>(approvedEvents);
        }

        public void clear() {
            receivedEvents.clear();
            approvedEvents.clear();
            logger.info("【テスト】イベントコレクターをクリア");
        }
    }

    /**
     * テスト用設定
     */
    @TestConfiguration
    static class TestConfig {
        @Bean
        public org.springframework.amqp.core.Queue testQueue() {
            return org.springframework.amqp.core.QueueBuilder.durable("test-event-queue").build();
        }

        @Bean
        public org.springframework.amqp.core.Binding testQueueBinding(
            org.springframework.amqp.core.Queue testQueue,
            org.springframework.amqp.core.TopicExchange financialEventsExchange) {
            return org.springframework.amqp.core.BindingBuilder
                .bind(testQueue)
                .to(financialEventsExchange)
                .with("financial.journalentry.*");
        }

        @Bean
        public TestEventCollector testEventCollector() {
            return new TestEventCollector();
        }
    }
}
