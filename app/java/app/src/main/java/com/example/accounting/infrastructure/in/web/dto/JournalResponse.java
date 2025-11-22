package com.example.accounting.infrastructure.in.web.dto;

import com.example.accounting.domain.model.Journal;
import com.example.accounting.domain.model.JournalEntry;
import com.example.accounting.domain.model.JournalLine;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;
import java.util.stream.Collectors;

/**
 * 仕訳レスポンス
 */
public record JournalResponse(
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
    public static JournalResponse from(Journal journal) {
        return new JournalResponse(
                journal.getJournalNo(),
                journal.getJournalDate(),
                journal.getInputDate(),
                journal.isSettlementFlag(),
                journal.isSingleEntryFlag(),
                journal.getJournalType(),
                journal.isRecurringFlag(),
                journal.getEmployeeCode(),
                journal.getDepartmentCode(),
                journal.isRedSlipFlag(),
                journal.getRedBlackVoucherNo(),
                journal.getEntries().stream()
                        .map(JournalEntryDto::from)
                        .collect(Collectors.toList())
        );
    }

    public record JournalEntryDto(
            Integer lineNumber,
            String description,
            List<JournalLineDto> lines
    ) {
        public static JournalEntryDto from(JournalEntry entry) {
            return new JournalEntryDto(
                    entry.getLineNumber(),
                    entry.getDescription(),
                    entry.getLines().stream()
                            .map(JournalLineDto::from)
                            .collect(Collectors.toList())
            );
        }
    }

    public record JournalLineDto(
            String debitCreditFlag,
            String currencyCode,
            BigDecimal exchangeRate,
            String departmentCode,
            String projectCode,
            String accountCode,
            String subAccountCode,
            BigDecimal amount,
            BigDecimal baseAmount,
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
        public static JournalLineDto from(JournalLine line) {
            return new JournalLineDto(
                    line.getDebitCreditFlag(),
                    line.getCurrencyCode(),
                    line.getExchangeRate(),
                    line.getDepartmentCode(),
                    line.getProjectCode(),
                    line.getAccountCode(),
                    line.getSubAccountCode(),
                    line.getAmount(),
                    line.getBaseAmount(),
                    line.getTaxType(),
                    line.getTaxRate(),
                    line.getTaxCalcType(),
                    line.getDueDate(),
                    line.isCashFlowFlag(),
                    line.getSegmentCode(),
                    line.getOffsetAccountCode(),
                    line.getOffsetSubAccountCode(),
                    line.getNoteCode(),
                    line.getNoteContent()
            );
        }
    }
}
