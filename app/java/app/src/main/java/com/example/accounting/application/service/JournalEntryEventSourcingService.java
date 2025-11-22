package com.example.accounting.application.service;

import com.example.accounting.domain.aggregate.JournalEntryAggregate;
import com.example.accounting.domain.aggregate.LineItem;
import com.example.accounting.domain.aggregate.DebitCredit;
import com.example.accounting.domain.event.DomainEvent;
import com.example.accounting.domain.repository.EventStoreRepository;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;
import java.util.UUID;

/**
 * イベントソーシング版仕訳サービス
 */
@Service
@RequiredArgsConstructor
public class JournalEntryEventSourcingService {
    private final EventStoreRepository eventStoreRepository;

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
        // イベント再生で現在の状態を復元
        List<DomainEvent> events = eventStoreRepository.getEvents(journalEntryId);
        if (events.isEmpty()) {
            throw new IllegalArgumentException("仕訳が見つかりません: " + journalEntryId);
        }

        JournalEntryAggregate aggregate = JournalEntryAggregate.replay(events);
        int currentVersion = aggregate.getVersion();

        // コマンド実行
        aggregate.approve(approvedBy, comment);

        // 新しいイベントを保存
        eventStoreRepository.save(
            journalEntryId,
            aggregate.getUncommittedEvents(),
            currentVersion
        );

        aggregate.markEventsAsCommitted();
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

        aggregate.markEventsAsCommitted();
    }

    /**
     * クエリ: 仕訳の現在の状態を取得
     *
     * @param journalEntryId 仕訳ID
     * @return 仕訳 Aggregate
     */
    public JournalEntryAggregate getJournalEntry(String journalEntryId) {
        List<DomainEvent> events = eventStoreRepository.getEvents(journalEntryId);
        if (events.isEmpty()) {
            throw new IllegalArgumentException("仕訳が見つかりません: " + journalEntryId);
        }
        return JournalEntryAggregate.replay(events);
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
