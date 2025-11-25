package com.example.accounting.infrastructure.out.persistence.dao;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;

/**
 * 仕訳明細 Read Model DAO
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class JournalEntryLineReadModel {
    /**
     * ID
     */
    private Long id;

    /**
     * 仕訳ID
     */
    private String journalEntryId;

    /**
     * 勘定科目コード
     */
    private String accountCode;

    /**
     * 借方・貸方区分
     */
    private String debitCredit;

    /**
     * 金額
     */
    private BigDecimal amount;
}
