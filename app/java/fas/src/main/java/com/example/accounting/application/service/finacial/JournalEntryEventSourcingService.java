package com.example.accounting.application.service.finacial;

import com.example.accounting.domain.aggregate.JournalEntryAggregate;
import com.example.accounting.domain.aggregate.LineItem;
import com.example.accounting.domain.aggregate.DebitCredit;
import com.example.accounting.domain.event.DomainEvent;
import com.example.accounting.domain.event.JournalEntryApprovedEvent;
import com.example.accounting.domain.event.JournalEntryCreatedEvent;
import com.example.accounting.domain.event.JournalEntryDeletedEvent;
import com.example.accounting.application.port.out.EventStoreRepository;
import com.example.accounting.application.port.out.SnapshotRepository;
import com.example.accounting.infrastructure.out.messaging.EventPublisher;
import lombok.Data;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

/**
 * イベントソーシング版仕訳サービス
 *
 * イベントソーシングでドメインイベントを永続化し、
 * RabbitMQ を通じて他のサービスにイベントをパブリッシュします。
 */
@Service
public class JournalEntryEventSourcingService {
    private static final Logger logger = LoggerFactory.getLogger(JournalEntryEventSourcingService.class);

    private final EventStoreRepository eventStoreRepository;
    private final SnapshotRepository snapshotRepository;
    private final EventPublisher eventPublisher;  // Optional: RabbitMQ が有効な場合のみ注入

    public JournalEntryEventSourcingService(
        EventStoreRepository eventStoreRepository,
        SnapshotRepository snapshotRepository,
        @Autowired(required = false) EventPublisher eventPublisher
    ) {
        this.eventStoreRepository = eventStoreRepository;
        this.snapshotRepository = snapshotRepository;
        this.eventPublisher = eventPublisher;
    }

    /** スナップショット作成間隔（イベント数） */
    private static final int SNAPSHOT_INTERVAL = 10;

    /**
     * 仕訳を作成
     *
     * @param entryDate 起票日
     * @param description 摘要
     * @param lineItems 明細行
     * @param userId ユーザーID
     * @return 仕訳ID
     */
    @Transactional
    public String createJournalEntry(
            LocalDate entryDate,
            String description,
            List<LineItemDto> lineItems,
            String userId) {

        String id = UUID.randomUUID().toString();

        // Aggregate 作成（コマンド実行）
        JournalEntryAggregate aggregate = JournalEntryAggregate.create(
            id,
            entryDate,
            description,
            lineItems.stream()
                    .map(dto -> new LineItem(
                        dto.getAccountCode(),
                        DebitCredit.valueOf(dto.getDebitCredit()),
                        dto.getAmount()
                    ))
                    .toList(),
            userId
        );

        // イベントをイベントストアに保存
        eventStoreRepository.save(
            id,
            aggregate.getUncommittedEvents(),
            0  // 新規作成なので expectedVersion = 0
        );

        // RabbitMQ にイベントをパブリッシュ
        publishEventsToMessageBroker(aggregate);

        aggregate.markEventsAsCommitted();

        return id;
    }

    /**
     * 仕訳を承認
     *
     * @param journalEntryId 仕訳ID
     * @param approvedBy 承認者ID
     * @param comment 承認コメント
     */
    @Transactional
    public void approveJournalEntry(String journalEntryId, String approvedBy, String comment) {
        // スナップショットから Aggregate を復元（最適化）
        JournalEntryAggregate aggregate = loadAggregate(journalEntryId);
        int currentVersion = aggregate.getVersion();

        // コマンド実行
        aggregate.approve(approvedBy, comment);

        // 新しいイベントを保存
        eventStoreRepository.save(
            journalEntryId,
            aggregate.getUncommittedEvents(),
            currentVersion
        );

        // RabbitMQ にイベントをパブリッシュ
        publishEventsToMessageBroker(aggregate);

        aggregate.markEventsAsCommitted();

        // スナップショット作成（一定間隔ごと）
        int newVersion = currentVersion + 1;
        if (newVersion % SNAPSHOT_INTERVAL == 0) {
            snapshotRepository.saveSnapshot(journalEntryId, newVersion, aggregate);
        }
    }

    /**
     * 仕訳を削除
     *
     * @param journalEntryId 仕訳ID
     * @param reason 削除理由
     * @param userId ユーザーID
     */
    @Transactional
    public void deleteJournalEntry(String journalEntryId, String reason, String userId) {
        List<DomainEvent> events = eventStoreRepository.getEvents(journalEntryId);
        if (events.isEmpty()) {
            throw new IllegalArgumentException("仕訳が見つかりません: " + journalEntryId);
        }

        JournalEntryAggregate aggregate = JournalEntryAggregate.replay(events);
        int currentVersion = aggregate.getVersion();

        aggregate.delete(reason, userId);

        eventStoreRepository.save(
            journalEntryId,
            aggregate.getUncommittedEvents(),
            currentVersion
        );

        // RabbitMQ にイベントをパブリッシュ
        publishEventsToMessageBroker(aggregate);

        aggregate.markEventsAsCommitted();
    }

    /**
     * クエリ: 仕訳の現在の状態を取得
     *
     * @param journalEntryId 仕訳ID
     * @return 仕訳 Aggregate
     */
    public JournalEntryAggregate getJournalEntry(String journalEntryId) {
        return loadAggregate(journalEntryId);
    }

    /**
     * Aggregate をスナップショットから効率的に読み込む
     *
     * @param journalEntryId 仕訳ID
     * @return Aggregate
     */
    private JournalEntryAggregate loadAggregate(String journalEntryId) {
        // スナップショットから復元を試みる
        Optional<SnapshotRepository.Snapshot> snapshotOpt =
            snapshotRepository.getLatestSnapshot(journalEntryId);

        if (snapshotOpt.isPresent()) {
            SnapshotRepository.Snapshot snapshot = snapshotOpt.get();
            JournalEntryAggregate aggregate = snapshot.aggregate();

            // スナップショット以降のイベントのみ再生
            List<DomainEvent> eventsAfterSnapshot =
                eventStoreRepository.getEventsAfterVersion(journalEntryId, snapshot.version());

            for (DomainEvent event : eventsAfterSnapshot) {
                aggregate.apply(event);
                aggregate.setVersion(aggregate.getVersion() + 1);
            }

            return aggregate;
        } else {
            // スナップショットがない場合は全イベントを再生
            List<DomainEvent> events = eventStoreRepository.getEvents(journalEntryId);
            if (events.isEmpty()) {
                throw new IllegalArgumentException("仕訳が見つかりません: " + journalEntryId);
            }
            return JournalEntryAggregate.replay(events);
        }
    }

    /**
     * RabbitMQ にイベントをパブリッシュ
     *
     * EventPublisher が注入されている場合（RabbitMQ が有効な場合）のみパブリッシュします。
     *
     * @param aggregate イベントを含む Aggregate
     */
    private void publishEventsToMessageBroker(JournalEntryAggregate aggregate) {
        if (eventPublisher == null) {
            logger.debug("EventPublisher が無効のため、イベントパブリッシュをスキップします");
            return;
        }

        for (DomainEvent event : aggregate.getUncommittedEvents()) {
            if (event instanceof JournalEntryCreatedEvent createdEvent) {
                eventPublisher.publishJournalEntryCreated(createdEvent);
                logger.info("イベントパブリッシュ: JournalEntryCreated - {}", createdEvent.getJournalEntryId());
            } else if (event instanceof JournalEntryApprovedEvent approvedEvent) {
                eventPublisher.publishJournalEntryApproved(approvedEvent);
                logger.info("イベントパブリッシュ: JournalEntryApproved - {}", approvedEvent.getJournalEntryId());
            } else if (event instanceof JournalEntryDeletedEvent deletedEvent) {
                eventPublisher.publishJournalEntryDeleted(deletedEvent);
                logger.info("イベントパブリッシュ: JournalEntryDeleted - {}", deletedEvent.getJournalEntryId());
            } else {
                logger.warn("未対応のイベントタイプ: {}", event.getClass().getSimpleName());
            }
        }
    }

    /**
     * 明細行 DTO
     */
    @Data
    public static class LineItemDto {
        private String accountCode;
        private String debitCredit;
        private BigDecimal amount;
    }
}
