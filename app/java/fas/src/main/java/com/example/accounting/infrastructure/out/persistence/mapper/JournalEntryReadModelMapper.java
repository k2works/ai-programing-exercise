package com.example.accounting.infrastructure.out.persistence.mapper;

import com.example.accounting.infrastructure.out.persistence.dao.JournalEntryReadModel;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;

/**
 * 仕訳 Read Model Mapper
 */
@Mapper
public interface JournalEntryReadModelMapper {

    /**
     * 仕訳を挿入
     */
    void insertJournalEntry(
        @Param("id") String id,
        @Param("entryDate") LocalDate entryDate,
        @Param("description") String description,
        @Param("status") String status,
        @Param("deleted") Boolean deleted,
        @Param("createdAt") LocalDateTime createdAt,
        @Param("updatedAt") LocalDateTime updatedAt,
        @Param("approvedBy") String approvedBy,
        @Param("approvalComment") String approvalComment
    );

    /**
     * 仕訳明細を挿入
     */
    void insertJournalEntryLine(
        @Param("journalEntryId") String journalEntryId,
        @Param("accountCode") String accountCode,
        @Param("debitCredit") String debitCredit,
        @Param("amount") BigDecimal amount
    );

    /**
     * 仕訳ステータスを更新
     */
    void updateJournalEntryStatus(
        @Param("id") String id,
        @Param("status") String status,
        @Param("updatedAt") LocalDateTime updatedAt,
        @Param("approvedBy") String approvedBy,
        @Param("approvalComment") String approvalComment
    );

    /**
     * 削除済みとしてマーク
     */
    void markAsDeleted(
        @Param("id") String id,
        @Param("updatedAt") LocalDateTime updatedAt
    );

    /**
     * ID で仕訳を取得
     */
    JournalEntryReadModel findById(@Param("id") String id);

    /**
     * すべての仕訳を取得
     */
    List<JournalEntryReadModel> findAll();

    /**
     * 起票日で仕訳を取得
     */
    List<JournalEntryReadModel> findByEntryDate(@Param("entryDate") LocalDate entryDate);

    /**
     * ステータスで仕訳を取得
     */
    List<JournalEntryReadModel> findByStatus(@Param("status") String status);

    /**
     * 削除されていない仕訳を取得
     */
    List<JournalEntryReadModel> findNotDeleted();
}
