package com.example.accounting.domain.aggregate;

import com.example.accounting.domain.event.DomainEvent;
import com.example.accounting.domain.event.JournalEntryApprovedEvent;
import com.example.accounting.domain.event.JournalEntryCreatedEvent;
import com.example.accounting.domain.event.JournalEntryDeletedEvent;
import lombok.Getter;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

/**
 * 仕訳 Aggregate
 *
 * イベントソーシングパターンで仕訳のライフサイクルを管理
 */
@Getter
public class JournalEntryAggregate {
    private String id;
    private LocalDate entryDate;
    private String description;
    private List<LineItem> lineItems = new ArrayList<>();
    private JournalEntryStatus status = JournalEntryStatus.DRAFT;
    private boolean deleted = false;

    // 未コミットイベント（まだイベントストアに保存されていない）
    private final List<DomainEvent> uncommittedEvents = new ArrayList<>();

    // バージョン（楽観的ロック用）
    private int version = 0;

    /**
     * イベント再生（Event Replay）
     *
     * @param events イベントリスト
     * @return 復元されたAggregate
     */
    public static JournalEntryAggregate replay(List<DomainEvent> events) {
        JournalEntryAggregate aggregate = new JournalEntryAggregate();
        for (DomainEvent event : events) {
            aggregate.apply(event);
            aggregate.version++;
        }
        return aggregate;
    }

    /**
     * コマンド: 仕訳を作成
     *
     * @param id 仕訳ID
     * @param entryDate 起票日
     * @param description 摘要
     * @param lineItems 明細行
     * @param userId ユーザーID
     * @return 作成されたAggregate
     */
    public static JournalEntryAggregate create(
            String id,
            LocalDate entryDate,
            String description,
            List<LineItem> lineItems,
            String userId) {

        // ビジネスルール検証
        if (lineItems == null || lineItems.isEmpty()) {
            throw new IllegalArgumentException("仕訳明細が必要です");
        }

        BigDecimal debitTotal = lineItems.stream()
                .filter(item -> item.getDebitCredit() == DebitCredit.DEBIT)
                .map(LineItem::getAmount)
                .reduce(BigDecimal.ZERO, BigDecimal::add);

        BigDecimal creditTotal = lineItems.stream()
                .filter(item -> item.getDebitCredit() == DebitCredit.CREDIT)
                .map(LineItem::getAmount)
                .reduce(BigDecimal.ZERO, BigDecimal::add);

        if (debitTotal.compareTo(creditTotal) != 0) {
            throw new IllegalArgumentException(
                "借方合計と貸方合計が一致しません: 借方=" + debitTotal + ", 貸方=" + creditTotal
            );
        }

        // イベント発行
        JournalEntryCreatedEvent event = JournalEntryCreatedEvent.builder()
                .journalEntryId(id)
                .entryDate(entryDate)
                .description(description)
                .lineItems(lineItems.stream()
                        .map(item -> JournalEntryCreatedEvent.JournalEntryLineItem.builder()
                                .accountCode(item.getAccountCode())
                                .debitCredit(JournalEntryCreatedEvent.DebitCredit.valueOf(
                                    item.getDebitCredit().name()))
                                .amount(item.getAmount())
                                .build())
                        .toList())
                .userId(userId)
                .occurredAt(LocalDateTime.now())
                .build();

        JournalEntryAggregate aggregate = new JournalEntryAggregate();
        aggregate.apply(event);
        aggregate.uncommittedEvents.add(event);

        return aggregate;
    }

    /**
     * コマンド: 仕訳を承認
     *
     * @param approvedBy 承認者ID
     * @param approvalComment 承認コメント
     */
    public void approve(String approvedBy, String approvalComment) {
        if (deleted) {
            throw new IllegalStateException("削除済みの仕訳は承認できません");
        }

        if (status == JournalEntryStatus.APPROVED) {
            throw new IllegalStateException("すでに承認済みです");
        }

        JournalEntryApprovedEvent event = JournalEntryApprovedEvent.builder()
                .journalEntryId(id)
                .approvedBy(approvedBy)
                .approvalComment(approvalComment)
                .occurredAt(LocalDateTime.now())
                .userId(approvedBy)
                .build();

        apply(event);
        uncommittedEvents.add(event);
    }

    /**
     * コマンド: 仕訳を削除
     *
     * @param reason 削除理由
     * @param userId ユーザーID
     */
    public void delete(String reason, String userId) {
        if (deleted) {
            throw new IllegalStateException("すでに削除済みです");
        }

        JournalEntryDeletedEvent event = JournalEntryDeletedEvent.builder()
                .journalEntryId(id)
                .reason(reason)
                .occurredAt(LocalDateTime.now())
                .userId(userId)
                .build();

        apply(event);
        uncommittedEvents.add(event);
    }

    /**
     * イベント適用（Apply）
     *
     * @param event ドメインイベント
     */
    private void apply(DomainEvent event) {
        if (event instanceof JournalEntryCreatedEvent e) {
            this.id = e.getJournalEntryId();
            this.entryDate = e.getEntryDate();
            this.description = e.getDescription();
            this.lineItems = e.getLineItems().stream()
                    .map(item -> new LineItem(
                        item.getAccountCode(),
                        DebitCredit.valueOf(item.getDebitCredit().name()),
                        item.getAmount()
                    ))
                    .toList();
            this.status = JournalEntryStatus.DRAFT;
        } else if (event instanceof JournalEntryApprovedEvent e) {
            this.status = JournalEntryStatus.APPROVED;
        } else if (event instanceof JournalEntryDeletedEvent e) {
            this.deleted = true;
        }
    }

    /**
     * 未コミットイベントをクリア
     */
    public void markEventsAsCommitted() {
        uncommittedEvents.clear();
    }
}
