package com.example.accounting.infrastructure.web.dto;

import com.example.accounting.application.model.financial.BalanceSheet;
import com.example.accounting.application.model.financial.BalanceSheetItem;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;
import java.util.stream.Collectors;

/**
 * 貸借対照表レスポンスDTO
 */
public record BalanceSheetResponse(
        LocalDate asOfDate,
        List<BalanceSheetItemDto> assets,
        List<BalanceSheetItemDto> liabilities,
        List<BalanceSheetItemDto> equity,
        BigDecimal totalAssets,
        BigDecimal totalLiabilities,
        BigDecimal totalEquity,
        BigDecimal totalLiabilitiesAndEquity
) {
    /**
     * ドメインモデルからDTOに変換
     */
    public static BalanceSheetResponse from(BalanceSheet balanceSheet) {
        return new BalanceSheetResponse(
                balanceSheet.getAsOfDate(),
                balanceSheet.getAssets().stream()
                        .map(BalanceSheetItemDto::from)
                        .collect(Collectors.toList()),
                balanceSheet.getLiabilities().stream()
                        .map(BalanceSheetItemDto::from)
                        .collect(Collectors.toList()),
                balanceSheet.getEquity().stream()
                        .map(BalanceSheetItemDto::from)
                        .collect(Collectors.toList()),
                balanceSheet.getTotalAssets(),
                balanceSheet.getTotalLiabilities(),
                balanceSheet.getTotalEquity(),
                balanceSheet.getTotalLiabilitiesAndEquity()
        );
    }

    /**
     * 貸借対照表明細DTO
     */
    public record BalanceSheetItemDto(
            String accountCode,
            String accountName,
            BigDecimal balance,
            BigDecimal ratio
    ) {
        public static BalanceSheetItemDto from(BalanceSheetItem item) {
            return new BalanceSheetItemDto(
                    item.getAccountCode(),
                    item.getAccountName(),
                    item.getBalance(),
                    item.getRatio()
            );
        }
    }
}
