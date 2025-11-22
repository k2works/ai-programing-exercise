package com.example.accounting.infrastructure.out.persistence.dao;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

/**
 * 仕訳 Read Model DAO
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class JournalEntryReadModel {
    /**
     * 仕訳ID
     */
    private String id;

    /**
     * 起票日
     */
    private LocalDate entryDate;

    /**
     * 摘要
     */
    private String description;

    /**
     * ステータス（DRAFT/APPROVED）
     */
    private String status;

    /**
     * 削除フラグ
     */
    private Boolean deleted;

    /**
     * 作成日時
     */
    private LocalDateTime createdAt;

    /**
     * 更新日時
     */
    private LocalDateTime updatedAt;

    /**
     * 承認者ID
     */
    private String approvedBy;

    /**
     * 承認コメント
     */
    private String approvalComment;

    /**
     * 明細行
     */
    @Builder.Default
    private List<JournalEntryLineReadModel> lineItems = new ArrayList<>();
}
