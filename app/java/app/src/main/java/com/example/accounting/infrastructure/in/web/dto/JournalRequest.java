package com.example.accounting.infrastructure.in.web.dto;

import java.time.LocalDate;
import java.util.List;

/**
 * 仕訳作成・更新リクエスト
 */
public record JournalRequest(
        String journalNo,
        LocalDate journalDate,
        LocalDate inputDate,
        boolean settlementFlag,
        boolean singleEntryFlag,
        Integer journalType,
        boolean recurringFlag,
        String employeeCode,
        String departmentCode,
        boolean redSlipFlag,
        String redBlackVoucherNo,
        List<JournalEntryDto> entries
) {
    public record JournalEntryDto(
            Integer lineNumber,
            String description,
            List<JournalLineDto> lines
    ) {
    }

    public record JournalLineDto(
            String debitCreditFlag,
            String currencyCode,
            String exchangeRate,
            String departmentCode,
            String projectCode,
            String accountCode,
            String subAccountCode,
            String amount,
            String baseAmount,
            String taxType,
            Integer taxRate,
            String taxCalcType,
            LocalDate dueDate,
            boolean cashFlowFlag,
            String segmentCode,
            String offsetAccountCode,
            String offsetSubAccountCode,
            String noteCode,
            String noteContent
    ) {
    }
}
