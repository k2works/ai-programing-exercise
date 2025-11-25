package com.example.accounting.infrastructure.in.seed;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;

/**
 * 財務会計システムの Seed データを提供するクラス
 *
 * D社（化粧品製造販売会社）の財務データを提供します。
 */
public class AccountingSeedData {

    // 勘定科目開始日(全科目共通)
    public static final LocalDate ACCOUNT_START_DATE = LocalDate.of(2021, 4, 1);

    /**
     * 勘定科目マスタデータを返す
     */
    public record AccountData(
        String accountCode,
        String accountName,
        String accountType,
        int level,
        LocalDate startDate
    ) {}

    public static List<AccountData> getAccounts() {
        return List.of(
            // 資産の部
            new AccountData("1", "資産", "ASSET", 1, ACCOUNT_START_DATE),
            new AccountData("11", "流動資産", "ASSET", 2, ACCOUNT_START_DATE),
            new AccountData("111", "現金預金", "ASSET", 3, ACCOUNT_START_DATE),
            new AccountData("112", "売掛金", "ASSET", 3, ACCOUNT_START_DATE),
            new AccountData("113", "売上債権", "ASSET", 3, ACCOUNT_START_DATE),
            new AccountData("114", "棚卸資産", "ASSET", 3, ACCOUNT_START_DATE),
            new AccountData("115", "その他流動資産", "ASSET", 3, ACCOUNT_START_DATE),
            new AccountData("12", "固定資産", "ASSET", 2, ACCOUNT_START_DATE),
            new AccountData("121", "有形固定資産", "ASSET", 3, ACCOUNT_START_DATE),
            new AccountData("1211", "建物及び構築物", "ASSET", 4, ACCOUNT_START_DATE),
            new AccountData("1212", "機械装置及び運搬具", "ASSET", 4, ACCOUNT_START_DATE),
            new AccountData("1213", "工具器具備品", "ASSET", 4, ACCOUNT_START_DATE),
            new AccountData("1214", "土地", "ASSET", 4, ACCOUNT_START_DATE),
            new AccountData("1215", "その他有形固定資産", "ASSET", 4, ACCOUNT_START_DATE),
            new AccountData("122", "無形固定資産", "ASSET", 3, ACCOUNT_START_DATE),
            new AccountData("123", "投資その他の資産", "ASSET", 3, ACCOUNT_START_DATE),

            // 負債の部
            new AccountData("2", "負債", "LIABILITY", 1, ACCOUNT_START_DATE),
            new AccountData("21", "流動負債", "LIABILITY", 2, ACCOUNT_START_DATE),
            new AccountData("211", "買掛金", "LIABILITY", 3, ACCOUNT_START_DATE),
            new AccountData("212", "短期借入金", "LIABILITY", 3, ACCOUNT_START_DATE),
            new AccountData("213", "未払金", "LIABILITY", 3, ACCOUNT_START_DATE),
            new AccountData("214", "未払法人税等", "LIABILITY", 3, ACCOUNT_START_DATE),
            new AccountData("215", "その他流動負債", "LIABILITY", 3, ACCOUNT_START_DATE),
            new AccountData("22", "固定負債", "LIABILITY", 2, ACCOUNT_START_DATE),
            new AccountData("221", "長期借入金", "LIABILITY", 3, ACCOUNT_START_DATE),
            new AccountData("222", "リース債務", "LIABILITY", 3, ACCOUNT_START_DATE),

            // 純資産の部
            new AccountData("3", "純資産", "EQUITY", 1, ACCOUNT_START_DATE),
            new AccountData("31", "資本金", "EQUITY", 2, ACCOUNT_START_DATE),
            new AccountData("32", "資本剰余金", "EQUITY", 2, ACCOUNT_START_DATE),
            new AccountData("33", "利益剰余金", "EQUITY", 2, ACCOUNT_START_DATE),

            // 収益の部
            new AccountData("4", "収益", "REVENUE", 1, ACCOUNT_START_DATE),
            new AccountData("41", "売上高", "REVENUE", 2, ACCOUNT_START_DATE),
            new AccountData("42", "営業外収益", "REVENUE", 2, ACCOUNT_START_DATE),
            new AccountData("43", "特別利益", "REVENUE", 2, ACCOUNT_START_DATE),

            // 費用の部
            new AccountData("5", "費用", "EXPENSE", 1, ACCOUNT_START_DATE),
            new AccountData("51", "売上原価", "EXPENSE", 2, ACCOUNT_START_DATE),
            new AccountData("52", "販売費及び一般管理費", "EXPENSE", 2, ACCOUNT_START_DATE),
            new AccountData("53", "営業外費用", "EXPENSE", 2, ACCOUNT_START_DATE),
            new AccountData("54", "特別損失", "EXPENSE", 2, ACCOUNT_START_DATE),
            new AccountData("55", "法人税等", "EXPENSE", 2, ACCOUNT_START_DATE),
            new AccountData("56", "当期純利益", "EXPENSE", 2, ACCOUNT_START_DATE)
        );
    }

    /**
     * 勘定科目構成マスタデータを返す
     * チルダ(~)連結による階層パスを使用
     */
    public record AccountStructureData(
        String accountCode,
        String accountPath
    ) {}

    public static List<AccountStructureData> getAccountStructures() {
        return List.of(
            // 資産の部
            new AccountStructureData("1", "1"),
            new AccountStructureData("11", "1~11"),
            new AccountStructureData("111", "1~11~111"),
            new AccountStructureData("112", "1~11~112"),
            new AccountStructureData("113", "1~11~113"),
            new AccountStructureData("114", "1~11~114"),
            new AccountStructureData("115", "1~11~115"),
            new AccountStructureData("12", "1~12"),
            new AccountStructureData("121", "1~12~121"),
            new AccountStructureData("1211", "1~12~121~1211"),
            new AccountStructureData("1212", "1~12~121~1212"),
            new AccountStructureData("1213", "1~12~121~1213"),
            new AccountStructureData("1214", "1~12~121~1214"),
            new AccountStructureData("1215", "1~12~121~1215"),
            new AccountStructureData("122", "1~12~122"),
            new AccountStructureData("123", "1~12~123"),

            // 負債の部
            new AccountStructureData("2", "2"),
            new AccountStructureData("21", "2~21"),
            new AccountStructureData("211", "2~21~211"),
            new AccountStructureData("212", "2~21~212"),
            new AccountStructureData("213", "2~21~213"),
            new AccountStructureData("214", "2~21~214"),
            new AccountStructureData("215", "2~21~215"),
            new AccountStructureData("22", "2~22"),
            new AccountStructureData("221", "2~22~221"),
            new AccountStructureData("222", "2~22~222"),

            // 純資産の部
            new AccountStructureData("3", "3"),
            new AccountStructureData("31", "3~31"),
            new AccountStructureData("32", "3~32"),
            new AccountStructureData("33", "3~33"),

            // 収益の部
            new AccountStructureData("4", "4"),
            new AccountStructureData("41", "4~41"),
            new AccountStructureData("42", "4~42"),
            new AccountStructureData("43", "4~43"),

            // 費用の部
            new AccountStructureData("5", "5"),
            new AccountStructureData("51", "5~51"),
            new AccountStructureData("52", "5~52"),
            new AccountStructureData("53", "5~53"),
            new AccountStructureData("54", "5~54"),
            new AccountStructureData("55", "5~55"),
            new AccountStructureData("56", "5~56")
        );
    }

    /**
     * 会計期間マスタデータを返す
     */
    public record AccountingPeriodData(
        int fiscalYear,
        String periodName,
        LocalDate startDate,
        LocalDate endDate,
        boolean isClosed
    ) {}

    public static List<AccountingPeriodData> getPeriods() {
        return List.of(
            new AccountingPeriodData(
                2021,
                "令和3年度",
                LocalDate.of(2021, 4, 1),
                LocalDate.of(2022, 3, 31),
                true
            ),
            new AccountingPeriodData(
                2022,
                "令和4年度",
                LocalDate.of(2022, 4, 1),
                LocalDate.of(2023, 3, 31),
                true
            )
        );
    }

    /**
     * 仕訳データを返す
     */
    public record JournalData(
        LocalDate journalDate,
        int fiscalYear,
        String description
    ) {}

    public static List<JournalData> getFY2021Journals() {
        return List.of(
            new JournalData(
                LocalDate.of(2022, 3, 31),
                2021,
                "令和3年度期末仕訳"
            )
        );
    }

    public static List<JournalData> getFY2022Journals() {
        return List.of(
            new JournalData(
                LocalDate.of(2023, 3, 31),
                2022,
                "令和4年度期末仕訳"
            )
        );
    }

    /**
     * 仕訳明細データを返す
     */
    public record JournalEntryData(
        String accountCode,
        BigDecimal debitAmount,
        BigDecimal creditAmount,
        String description
    ) {}

    public static List<JournalEntryData> getFY2021Entries() {
        return List.of(
            // 貸借対照表
            new JournalEntryData("11", new BigDecimal("2676193000"), BigDecimal.ZERO, "流動資産"),
            new JournalEntryData("12", new BigDecimal("186973000"), BigDecimal.ZERO, "固定資産"),
            new JournalEntryData("21", BigDecimal.ZERO, new BigDecimal("851394000"), "流動負債"),
            new JournalEntryData("22", BigDecimal.ZERO, new BigDecimal("22500000"), "固定負債"),
            new JournalEntryData("31", BigDecimal.ZERO, new BigDecimal("100000000"), "資本金"),
            new JournalEntryData("33", BigDecimal.ZERO, new BigDecimal("1889272000"), "利益剰余金"),

            // 損益計算書
            new JournalEntryData("51", new BigDecimal("2185856000"), BigDecimal.ZERO, "売上原価"),
            new JournalEntryData("52", new BigDecimal("2625222000"), BigDecimal.ZERO, "販売費及び一般管理費"),
            new JournalEntryData("53", new BigDecimal("2676000"), BigDecimal.ZERO, "営業外費用"),
            new JournalEntryData("55", new BigDecimal("331059000"), BigDecimal.ZERO, "法人税等"),
            new JournalEntryData("41", BigDecimal.ZERO, new BigDecimal("5796105000"), "売上高"),
            new JournalEntryData("42", BigDecimal.ZERO, new BigDecimal("368000"), "営業外収益"),
            new JournalEntryData("56", BigDecimal.ZERO, new BigDecimal("651660000"), "当期純利益")
        );
    }

    public static List<JournalEntryData> getFY2022Entries() {
        return List.of(
            // 貸借対照表
            new JournalEntryData("11", new BigDecimal("2777545000"), BigDecimal.ZERO, "流動資産"),
            new JournalEntryData("12", new BigDecimal("197354000"), BigDecimal.ZERO, "固定資産"),
            new JournalEntryData("21", BigDecimal.ZERO, new BigDecimal("640513000"), "流動負債"),
            new JournalEntryData("22", BigDecimal.ZERO, new BigDecimal("27153000"), "固定負債"),
            new JournalEntryData("31", BigDecimal.ZERO, new BigDecimal("100000000"), "資本金"),
            new JournalEntryData("33", BigDecimal.ZERO, new BigDecimal("2207233000"), "利益剰余金"),

            // 損益計算書
            new JournalEntryData("51", new BigDecimal("1743821000"), BigDecimal.ZERO, "売上原価"),
            new JournalEntryData("52", new BigDecimal("2277050000"), BigDecimal.ZERO, "販売費及び一般管理費"),
            new JournalEntryData("53", new BigDecimal("1613000"), BigDecimal.ZERO, "営業外費用"),
            new JournalEntryData("55", new BigDecimal("169072000"), BigDecimal.ZERO, "法人税等"),
            new JournalEntryData("41", BigDecimal.ZERO, new BigDecimal("4547908000"), "売上高"),
            new JournalEntryData("42", BigDecimal.ZERO, new BigDecimal("11608000"), "営業外収益"),
            new JournalEntryData("56", BigDecimal.ZERO, new BigDecimal("367960000"), "当期純利益")
        );
    }

    /**
     * 日次勘定科目残高データを返す
     */
    public record DailyBalanceData(
        LocalDate balanceDate,
        String accountCode,
        String auxiliaryCode,
        String departmentCode,
        String projectCode,
        boolean isClosingEntry,
        BigDecimal debitAmount,
        BigDecimal creditAmount
    ) {}

    public static List<DailyBalanceData> getFY2021DailyBalances() {
        LocalDate endDate = LocalDate.of(2022, 3, 31);
        return List.of(
            new DailyBalanceData(endDate, "11", "", "", "", true, new BigDecimal("2676193000"), BigDecimal.ZERO),
            new DailyBalanceData(endDate, "12", "", "", "", true, new BigDecimal("186973000"), BigDecimal.ZERO),
            new DailyBalanceData(endDate, "21", "", "", "", true, BigDecimal.ZERO, new BigDecimal("851394000")),
            new DailyBalanceData(endDate, "22", "", "", "", true, BigDecimal.ZERO, new BigDecimal("22500000")),
            new DailyBalanceData(endDate, "31", "", "", "", true, BigDecimal.ZERO, new BigDecimal("100000000")),
            new DailyBalanceData(endDate, "33", "", "", "", true, BigDecimal.ZERO, new BigDecimal("1889272000")),
            new DailyBalanceData(endDate, "51", "", "", "", true, new BigDecimal("2185856000"), BigDecimal.ZERO),
            new DailyBalanceData(endDate, "52", "", "", "", true, new BigDecimal("2625222000"), BigDecimal.ZERO),
            new DailyBalanceData(endDate, "53", "", "", "", true, new BigDecimal("2676000"), BigDecimal.ZERO),
            new DailyBalanceData(endDate, "55", "", "", "", true, new BigDecimal("331059000"), BigDecimal.ZERO),
            new DailyBalanceData(endDate, "41", "", "", "", true, BigDecimal.ZERO, new BigDecimal("5796105000")),
            new DailyBalanceData(endDate, "42", "", "", "", true, BigDecimal.ZERO, new BigDecimal("368000")),
            new DailyBalanceData(endDate, "56", "", "", "", true, BigDecimal.ZERO, new BigDecimal("651660000"))
        );
    }

    public static List<DailyBalanceData> getFY2022DailyBalances() {
        LocalDate endDate = LocalDate.of(2023, 3, 31);
        return List.of(
            new DailyBalanceData(endDate, "11", "", "", "", true, new BigDecimal("2777545000"), BigDecimal.ZERO),
            new DailyBalanceData(endDate, "12", "", "", "", true, new BigDecimal("197354000"), BigDecimal.ZERO),
            new DailyBalanceData(endDate, "21", "", "", "", true, BigDecimal.ZERO, new BigDecimal("640513000")),
            new DailyBalanceData(endDate, "22", "", "", "", true, BigDecimal.ZERO, new BigDecimal("27153000")),
            new DailyBalanceData(endDate, "31", "", "", "", true, BigDecimal.ZERO, new BigDecimal("100000000")),
            new DailyBalanceData(endDate, "33", "", "", "", true, BigDecimal.ZERO, new BigDecimal("2207233000")),
            new DailyBalanceData(endDate, "51", "", "", "", true, new BigDecimal("1743821000"), BigDecimal.ZERO),
            new DailyBalanceData(endDate, "52", "", "", "", true, new BigDecimal("2277050000"), BigDecimal.ZERO),
            new DailyBalanceData(endDate, "53", "", "", "", true, new BigDecimal("1613000"), BigDecimal.ZERO),
            new DailyBalanceData(endDate, "55", "", "", "", true, new BigDecimal("169072000"), BigDecimal.ZERO),
            new DailyBalanceData(endDate, "41", "", "", "", true, BigDecimal.ZERO, new BigDecimal("4547908000")),
            new DailyBalanceData(endDate, "42", "", "", "", true, BigDecimal.ZERO, new BigDecimal("11608000")),
            new DailyBalanceData(endDate, "56", "", "", "", true, BigDecimal.ZERO, new BigDecimal("367960000"))
        );
    }

    /**
     * 月次勘定科目残高データを返す
     */
    public record MonthlyBalanceData(
        int fiscalYear,
        int month,
        String accountCode,
        String auxiliaryCode,
        String departmentCode,
        String projectCode,
        BigDecimal beginningBalance,
        BigDecimal debitAmount,
        BigDecimal creditAmount,
        BigDecimal endingBalance
    ) {}

    public static List<MonthlyBalanceData> getFY2021MonthlyBalances() {
        return List.of(
            // 資産(借方残高)
            new MonthlyBalanceData(2021, 3, "11", "", "", "", BigDecimal.ZERO, new BigDecimal("2676193000"), BigDecimal.ZERO, new BigDecimal("2676193000")),
            new MonthlyBalanceData(2021, 3, "12", "", "", "", BigDecimal.ZERO, new BigDecimal("186973000"), BigDecimal.ZERO, new BigDecimal("186973000")),

            // 負債(貸方残高)
            new MonthlyBalanceData(2021, 3, "21", "", "", "", BigDecimal.ZERO, BigDecimal.ZERO, new BigDecimal("851394000"), new BigDecimal("851394000")),
            new MonthlyBalanceData(2021, 3, "22", "", "", "", BigDecimal.ZERO, BigDecimal.ZERO, new BigDecimal("22500000"), new BigDecimal("22500000")),

            // 純資産(貸方残高)
            new MonthlyBalanceData(2021, 3, "31", "", "", "", BigDecimal.ZERO, BigDecimal.ZERO, new BigDecimal("100000000"), new BigDecimal("100000000")),
            new MonthlyBalanceData(2021, 3, "33", "", "", "", BigDecimal.ZERO, BigDecimal.ZERO, new BigDecimal("1889272000"), new BigDecimal("1889272000")),

            // 費用(借方残高)
            new MonthlyBalanceData(2021, 3, "51", "", "", "", BigDecimal.ZERO, new BigDecimal("2185856000"), BigDecimal.ZERO, new BigDecimal("2185856000")),
            new MonthlyBalanceData(2021, 3, "52", "", "", "", BigDecimal.ZERO, new BigDecimal("2625222000"), BigDecimal.ZERO, new BigDecimal("2625222000")),
            new MonthlyBalanceData(2021, 3, "53", "", "", "", BigDecimal.ZERO, new BigDecimal("2676000"), BigDecimal.ZERO, new BigDecimal("2676000")),
            new MonthlyBalanceData(2021, 3, "55", "", "", "", BigDecimal.ZERO, new BigDecimal("331059000"), BigDecimal.ZERO, new BigDecimal("331059000")),

            // 収益(貸方残高)
            new MonthlyBalanceData(2021, 3, "41", "", "", "", BigDecimal.ZERO, BigDecimal.ZERO, new BigDecimal("5796105000"), new BigDecimal("5796105000")),
            new MonthlyBalanceData(2021, 3, "42", "", "", "", BigDecimal.ZERO, BigDecimal.ZERO, new BigDecimal("368000"), new BigDecimal("368000")),
            new MonthlyBalanceData(2021, 3, "56", "", "", "", BigDecimal.ZERO, BigDecimal.ZERO, new BigDecimal("651660000"), new BigDecimal("651660000"))
        );
    }

    public static List<MonthlyBalanceData> getFY2022MonthlyBalances() {
        return List.of(
            // 資産(借方残高)
            new MonthlyBalanceData(2022, 3, "11", "", "", "", BigDecimal.ZERO, new BigDecimal("2777545000"), BigDecimal.ZERO, new BigDecimal("2777545000")),
            new MonthlyBalanceData(2022, 3, "12", "", "", "", BigDecimal.ZERO, new BigDecimal("197354000"), BigDecimal.ZERO, new BigDecimal("197354000")),

            // 負債(貸方残高)
            new MonthlyBalanceData(2022, 3, "21", "", "", "", BigDecimal.ZERO, BigDecimal.ZERO, new BigDecimal("640513000"), new BigDecimal("640513000")),
            new MonthlyBalanceData(2022, 3, "22", "", "", "", BigDecimal.ZERO, BigDecimal.ZERO, new BigDecimal("27153000"), new BigDecimal("27153000")),

            // 純資産(貸方残高)
            new MonthlyBalanceData(2022, 3, "31", "", "", "", BigDecimal.ZERO, BigDecimal.ZERO, new BigDecimal("100000000"), new BigDecimal("100000000")),
            new MonthlyBalanceData(2022, 3, "33", "", "", "", BigDecimal.ZERO, BigDecimal.ZERO, new BigDecimal("2207233000"), new BigDecimal("2207233000")),

            // 費用(借方残高)
            new MonthlyBalanceData(2022, 3, "51", "", "", "", BigDecimal.ZERO, new BigDecimal("1743821000"), BigDecimal.ZERO, new BigDecimal("1743821000")),
            new MonthlyBalanceData(2022, 3, "52", "", "", "", BigDecimal.ZERO, new BigDecimal("2277050000"), BigDecimal.ZERO, new BigDecimal("2277050000")),
            new MonthlyBalanceData(2022, 3, "53", "", "", "", BigDecimal.ZERO, new BigDecimal("1613000"), BigDecimal.ZERO, new BigDecimal("1613000")),
            new MonthlyBalanceData(2022, 3, "55", "", "", "", BigDecimal.ZERO, new BigDecimal("169072000"), BigDecimal.ZERO, new BigDecimal("169072000")),

            // 収益(貸方残高)
            new MonthlyBalanceData(2022, 3, "41", "", "", "", BigDecimal.ZERO, BigDecimal.ZERO, new BigDecimal("4547908000"), new BigDecimal("4547908000")),
            new MonthlyBalanceData(2022, 3, "42", "", "", "", BigDecimal.ZERO, BigDecimal.ZERO, new BigDecimal("11608000"), new BigDecimal("11608000")),
            new MonthlyBalanceData(2022, 3, "56", "", "", "", BigDecimal.ZERO, BigDecimal.ZERO, new BigDecimal("367960000"), new BigDecimal("367960000"))
        );
    }
}
