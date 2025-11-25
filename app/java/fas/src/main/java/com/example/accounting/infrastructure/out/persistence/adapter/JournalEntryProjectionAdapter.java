package com.example.accounting.infrastructure.out.persistence.adapter;

import com.example.accounting.application.port.out.JournalEntryProjection;
import com.example.accounting.domain.event.JournalEntryApprovedEvent;
import com.example.accounting.domain.event.JournalEntryCreatedEvent;
import com.example.accounting.domain.event.JournalEntryDeletedEvent;
import com.example.accounting.infrastructure.out.persistence.mapper.JournalEntryReadModelMapper;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.transaction.event.TransactionPhase;
import org.springframework.transaction.event.TransactionalEventListener;

/**
 * 仕訳 Projection Adapter
 *
 * イベントストアから Read Model を構築
 */
@Component
@RequiredArgsConstructor
@Slf4j
public class JournalEntryProjectionAdapter implements JournalEntryProjection {
    private final JournalEntryReadModelMapper readModelMapper;

    /**
     * 仕訳作成イベントハンドラー
     *
     * @param event 仕訳作成イベント
     */
    @Override
    @TransactionalEventListener(phase = TransactionPhase.AFTER_COMMIT)
    @Transactional(propagation = Propagation.REQUIRES_NEW)
    public void handleJournalEntryCreated(JournalEntryCreatedEvent event) {
        log.info("Projecting JournalEntryCreatedEvent: {}", event.getJournalEntryId());

        // Read Model に保存
        readModelMapper.insertJournalEntry(
            event.getJournalEntryId(),
            event.getEntryDate(),
            event.getDescription(),
            "DRAFT",
            false,
            event.getOccurredAt(),
            event.getOccurredAt(),
            null,
            null
        );

        // 仕訳明細も保存
        for (var lineItem : event.getLineItems()) {
            readModelMapper.insertJournalEntryLine(
                event.getJournalEntryId(),
                lineItem.getAccountCode(),
                lineItem.getDebitCredit().name(),
                lineItem.getAmount()
            );
        }
    }

    /**
     * 仕訳承認イベントハンドラー
     *
     * @param event 仕訳承認イベント
     */
    @Override
    @TransactionalEventListener(phase = TransactionPhase.AFTER_COMMIT)
    @Transactional(propagation = Propagation.REQUIRES_NEW)
    public void handleJournalEntryApproved(JournalEntryApprovedEvent event) {
        log.info("Projecting JournalEntryApprovedEvent: {}", event.getJournalEntryId());

        readModelMapper.updateJournalEntryStatus(
            event.getJournalEntryId(),
            "APPROVED",
            event.getOccurredAt(),
            event.getApprovedBy(),
            event.getApprovalComment()
        );
    }

    /**
     * 仕訳削除イベントハンドラー
     *
     * @param event 仕訳削除イベント
     */
    @Override
    @TransactionalEventListener(phase = TransactionPhase.AFTER_COMMIT)
    @Transactional(propagation = Propagation.REQUIRES_NEW)
    public void handleJournalEntryDeleted(JournalEntryDeletedEvent event) {
        log.info("Projecting JournalEntryDeletedEvent: {}", event.getJournalEntryId());

        readModelMapper.markAsDeleted(
            event.getJournalEntryId(),
            event.getOccurredAt()
        );
    }
}
