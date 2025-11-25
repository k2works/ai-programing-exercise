package com.example.accounting.domain.aggregate;

/**
 * 仕訳ステータス
 */
public enum JournalEntryStatus {
    /**
     * 下書き
     */
    DRAFT,

    /**
     * 承認済み
     */
    APPROVED
}
