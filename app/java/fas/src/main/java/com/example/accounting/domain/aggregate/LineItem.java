package com.example.accounting.domain.aggregate;

import lombok.AllArgsConstructor;
import lombok.Getter;

import java.math.BigDecimal;

/**
 * 仕訳明細行
 */
@Getter
@AllArgsConstructor
public class LineItem {
    /**
     * 勘定科目コード
     */
    private final String accountCode;

    /**
     * 借方・貸方区分
     */
    private final DebitCredit debitCredit;

    /**
     * 金額
     */
    private final BigDecimal amount;
}
