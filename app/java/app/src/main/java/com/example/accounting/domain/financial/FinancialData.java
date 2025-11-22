package com.example.accounting.domain.financial;

import java.math.BigDecimal;
import java.util.List;

/**
 * 財務データを表現する値オブジェクト
 *
 * このクラスは不変であり、スレッドセーフです。
 */
public record FinancialData(
    int fiscalYear,
    BigDecimal sales,
    BigDecimal operatingProfit,
    BigDecimal totalAssets,
    BigDecimal tangibleFixedAssets,
    BigDecimal currentAssets,
    BigDecimal currentLiabilities,
    BigDecimal quickAssets,
    BigDecimal equity
) {

    /**
     * 仕訳データから財務データを生成
     *
     * @param fiscalYear 会計年度
     * @param entries 仕訳明細データのリスト
     * @return 計算された財務データ
     */
    public static FinancialData fromJournalEntries(
        int fiscalYear,
        List<JournalEntryData> entries
    ) {
        AccountAmountExtractor extractor = new AccountAmountExtractor(entries);

        return new FinancialData(
            fiscalYear,
            extractor.get("41"),  // 売上高
            calculateOperatingProfit(extractor),
            calculateTotalAssets(extractor),
            extractor.get("121"),  // 有形固定資産
            extractor.get("11"),   // 流動資産
            extractor.get("21"),   // 流動負債
            calculateQuickAssets(extractor),
            calculateEquity(extractor)
        );
    }

    /**
     * 営業利益を計算
     */
    private static BigDecimal calculateOperatingProfit(AccountAmountExtractor extractor) {
        return extractor.get("41")  // 売上高
            .subtract(extractor.get("51"))  // 売上原価
            .subtract(extractor.get("52")); // 販売費及び一般管理費
    }

    /**
     * 総資産を計算
     */
    private static BigDecimal calculateTotalAssets(AccountAmountExtractor extractor) {
        return extractor.get("11")  // 流動資産
            .add(extractor.get("12"));  // 固定資産
    }

    /**
     * 当座資産を計算（流動資産 - 棚卸資産）
     */
    private static BigDecimal calculateQuickAssets(AccountAmountExtractor extractor) {
        return extractor.get("11")  // 流動資産
            .subtract(extractor.get("114"));  // 棚卸資産
    }

    /**
     * 純資産を計算
     */
    private static BigDecimal calculateEquity(AccountAmountExtractor extractor) {
        return extractor.get("31")  // 資本金
            .add(extractor.get("33"));  // 利益剰余金
    }

    /**
     * 勘定科目コードから金額を抽出するヘルパークラス
     */
    private static class AccountAmountExtractor {
        private final List<JournalEntryData> entries;

        AccountAmountExtractor(List<JournalEntryData> entries) {
            this.entries = entries;
        }

        BigDecimal get(String accountCode) {
            return entries.stream()
                .filter(e -> e.accountCode().equals(accountCode))
                .findFirst()
                .map(e -> e.debitAmount().compareTo(BigDecimal.ZERO) > 0
                    ? e.debitAmount()
                    : e.creditAmount())
                .orElse(BigDecimal.ZERO);
        }
    }

    /**
     * 仕訳明細データ
     */
    public record JournalEntryData(
        String accountCode,
        BigDecimal debitAmount,
        BigDecimal creditAmount
    ) {}
}
