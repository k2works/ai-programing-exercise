package com.example.accounting.infrastructure.web.controller;

import com.example.accounting.application.service.JournalEntryEventSourcingService;
import com.example.accounting.domain.aggregate.JournalEntryAggregate;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;

/**
 * イベントソーシング版仕訳 REST API Controller
 */
@RestController
@RequestMapping("/api/v1/journal-entries-es")
@RequiredArgsConstructor
@Tag(name = "Journal Entry Event Sourcing", description = "イベントソーシング版仕訳API")
public class JournalEntryEventSourcingController {
    private final JournalEntryEventSourcingService journalEntryService;

    /**
     * 仕訳を作成する
     *
     * @param request 作成リクエスト
     * @return 作成された仕訳ID
     */
    @PostMapping
    @Operation(summary = "仕訳を作成する（イベントソーシング）")
    public ResponseEntity<CreateJournalEntryResponse> createJournalEntry(
            @RequestBody CreateJournalEntryRequest request) {

        String id = journalEntryService.createJournalEntry(
            request.getEntryDate(),
            request.getDescription(),
            request.getLineItems().stream()
                    .map(item -> {
                        var dto = new JournalEntryEventSourcingService.LineItemDto();
                        dto.setAccountCode(item.getAccountCode());
                        dto.setDebitCredit(item.getDebitCredit());
                        dto.setAmount(item.getAmount());
                        return dto;
                    })
                    .toList(),
            request.getUserId()
        );

        return ResponseEntity.status(HttpStatus.CREATED)
                .body(new CreateJournalEntryResponse(id));
    }

    /**
     * 仕訳を承認する
     *
     * @param id 仕訳ID
     * @param request 承認リクエスト
     * @return 204 No Content
     */
    @PostMapping("/{id}/approve")
    @Operation(summary = "仕訳を承認する")
    public ResponseEntity<Void> approveJournalEntry(
            @PathVariable String id,
            @RequestBody ApproveJournalEntryRequest request) {

        journalEntryService.approveJournalEntry(id, request.getApprovedBy(), request.getComment());
        return ResponseEntity.noContent().build();
    }

    /**
     * 仕訳を削除する
     *
     * @param id 仕訳ID
     * @param request 削除リクエスト
     * @return 204 No Content
     */
    @DeleteMapping("/{id}")
    @Operation(summary = "仕訳を削除する")
    public ResponseEntity<Void> deleteJournalEntry(
            @PathVariable String id,
            @RequestBody DeleteJournalEntryRequest request) {

        journalEntryService.deleteJournalEntry(id, request.getReason(), request.getUserId());
        return ResponseEntity.noContent().build();
    }

    /**
     * 仕訳を取得する（イベント再生）
     *
     * @param id 仕訳ID
     * @return 仕訳情報
     */
    @GetMapping("/{id}")
    @Operation(summary = "仕訳を取得する（イベント再生）")
    public ResponseEntity<JournalEntryResponse> getJournalEntry(@PathVariable String id) {
        JournalEntryAggregate aggregate = journalEntryService.getJournalEntry(id);

        JournalEntryResponse response = new JournalEntryResponse();
        response.setId(aggregate.getId());
        response.setEntryDate(aggregate.getEntryDate());
        response.setDescription(aggregate.getDescription());
        response.setStatus(aggregate.getStatus().name());
        response.setDeleted(aggregate.isDeleted());
        response.setLineItems(aggregate.getLineItems().stream()
                .map(item -> {
                    var lineItem = new JournalEntryResponse.LineItem();
                    lineItem.setAccountCode(item.getAccountCode());
                    lineItem.setDebitCredit(item.getDebitCredit().name());
                    lineItem.setAmount(item.getAmount());
                    return lineItem;
                })
                .toList());

        return ResponseEntity.ok(response);
    }

    /**
     * 仕訳作成リクエスト
     */
    @Data
    public static class CreateJournalEntryRequest {
        private LocalDate entryDate;
        private String description;
        private List<LineItemRequest> lineItems;
        private String userId;

        /**
         * 明細行リクエスト
         */
        @Data
        public static class LineItemRequest {
            private String accountCode;
            private String debitCredit;
            private BigDecimal amount;
        }
    }

    /**
     * 仕訳作成レスポンス
     */
    @Data
    @AllArgsConstructor
    public static class CreateJournalEntryResponse {
        private String id;
    }

    /**
     * 仕訳承認リクエスト
     */
    @Data
    public static class ApproveJournalEntryRequest {
        private String approvedBy;
        private String comment;
    }

    /**
     * 仕訳削除リクエスト
     */
    @Data
    public static class DeleteJournalEntryRequest {
        private String reason;
        private String userId;
    }

    /**
     * 仕訳レスポンス
     */
    @Data
    public static class JournalEntryResponse {
        private String id;
        private LocalDate entryDate;
        private String description;
        private String status;
        private boolean deleted;
        private List<LineItem> lineItems;

        /**
         * 明細行
         */
        @Data
        public static class LineItem {
            private String accountCode;
            private String debitCredit;
            private BigDecimal amount;
        }
    }
}
