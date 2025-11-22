package com.example.accounting.infrastructure.in.web.dto.financial;

import com.example.accounting.domain.model.financial.IncomeStatement;
import com.example.accounting.domain.model.financial.IncomeStatementItem;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;
import java.util.stream.Collectors;

/**
 * 損益計算書レスポンスDTO
 */
public record IncomeStatementResponse(
        LocalDate fromDate,
        LocalDate toDate,
        List<IncomeStatementItemDto> revenues,
        List<IncomeStatementItemDto> expenses,
        BigDecimal grossProfit,
        BigDecimal operatingIncome,
        BigDecimal netIncome,
        BigDecimal totalRevenues,
        BigDecimal totalExpenses
) {
    /**
     * ドメインモデルからDTOに変換
     */
    public static IncomeStatementResponse from(IncomeStatement incomeStatement) {
        return new IncomeStatementResponse(
                incomeStatement.getFromDate(),
                incomeStatement.getToDate(),
                incomeStatement.getRevenues().stream()
                        .map(IncomeStatementItemDto::from)
                        .collect(Collectors.toList()),
                incomeStatement.getExpenses().stream()
                        .map(IncomeStatementItemDto::from)
                        .collect(Collectors.toList()),
                incomeStatement.getGrossProfit(),
                incomeStatement.getOperatingIncome(),
                incomeStatement.getNetIncome(),
                incomeStatement.getTotalRevenues(),
                incomeStatement.getTotalExpenses()
        );
    }

    /**
     * 損益計算書明細DTO
     */
    public record IncomeStatementItemDto(
            String accountCode,
            String accountName,
            BigDecimal balance,
            BigDecimal percentage
    ) {
        public static IncomeStatementItemDto from(IncomeStatementItem item) {
            return new IncomeStatementItemDto(
                    item.getAccountCode(),
                    item.getAccountName(),
                    item.getBalance(),
                    item.getPercentage()
            );
        }
    }
}
