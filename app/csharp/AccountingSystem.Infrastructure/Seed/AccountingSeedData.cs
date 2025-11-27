namespace AccountingSystem.Infrastructure.Seed;

/// <summary>
/// 財務会計システムの Seed データを提供するクラス
/// D社（化粧品製造販売）の事例データを基にしています
/// </summary>
public static class AccountingSeedData
{
    /// <summary>
    /// 勘定科目開始日（全科目共通）
    /// </summary>
    public static readonly DateOnly AccountStartDate = new(2021, 4, 1);

    #region データ型定義

    /// <summary>
    /// 勘定科目マスタデータ
    /// </summary>
    public record AccountData(
        string AccountCode,
        string AccountName,
        string AccountType,
        int Level,
        DateOnly StartDate
    );

    /// <summary>
    /// 勘定科目構成マスタデータ
    /// </summary>
    public record AccountStructureData(
        string AccountCode,
        string AccountPath
    );

    /// <summary>
    /// 会計期間マスタデータ
    /// </summary>
    public record PeriodData(
        int FiscalYear,
        string PeriodName,
        DateOnly StartDate,
        DateOnly EndDate,
        bool IsClosed
    );

    /// <summary>
    /// 仕訳データ
    /// </summary>
    public record JournalData(
        DateOnly JournalDate,
        int FiscalYear,
        string Description
    );

    /// <summary>
    /// 仕訳明細データ
    /// </summary>
    public record JournalEntryData(
        string AccountCode,
        decimal DebitAmount,
        decimal CreditAmount,
        string Description
    );

    /// <summary>
    /// 日次勘定科目残高データ
    /// </summary>
    public record DailyBalanceData(
        string AccountCode,
        DateOnly BalanceDate,
        decimal DebitAmount,
        decimal CreditAmount
    );

    /// <summary>
    /// 月次勘定科目残高データ
    /// </summary>
    public record MonthlyBalanceData(
        string AccountCode,
        int FiscalYear,
        int Month,
        decimal OpeningBalance,
        decimal DebitAmount,
        decimal CreditAmount,
        decimal ClosingBalance
    );

    #endregion

    #region 勘定科目マスタ

    /// <summary>
    /// 勘定科目マスタデータ（42科目）
    /// AccountType は PostgreSQL の account_type enum に合わせて日本語で定義
    /// </summary>
    public static List<AccountData> GetAccounts()
    {
        return new List<AccountData>
        {
            // 資産の部
            new("1", "資産", "資産", 1, AccountStartDate),
            new("11", "流動資産", "資産", 2, AccountStartDate),
            new("111", "現金預金", "資産", 3, AccountStartDate),
            new("112", "売掛金", "資産", 3, AccountStartDate),
            new("113", "売上債権", "資産", 3, AccountStartDate),
            new("114", "棚卸資産", "資産", 3, AccountStartDate),
            new("115", "その他流動資産", "資産", 3, AccountStartDate),
            new("12", "固定資産", "資産", 2, AccountStartDate),
            new("121", "有形固定資産", "資産", 3, AccountStartDate),
            new("1211", "建物及び構築物", "資産", 4, AccountStartDate),
            new("1212", "機械装置及び運搬具", "資産", 4, AccountStartDate),
            new("1213", "工具器具備品", "資産", 4, AccountStartDate),
            new("1214", "土地", "資産", 4, AccountStartDate),
            new("1215", "その他有形固定資産", "資産", 4, AccountStartDate),
            new("122", "無形固定資産", "資産", 3, AccountStartDate),
            new("123", "投資その他の資産", "資産", 3, AccountStartDate),

            // 負債の部
            new("2", "負債", "負債", 1, AccountStartDate),
            new("21", "流動負債", "負債", 2, AccountStartDate),
            new("211", "買掛金", "負債", 3, AccountStartDate),
            new("212", "短期借入金", "負債", 3, AccountStartDate),
            new("213", "未払金", "負債", 3, AccountStartDate),
            new("214", "未払法人税等", "負債", 3, AccountStartDate),
            new("215", "その他流動負債", "負債", 3, AccountStartDate),
            new("22", "固定負債", "負債", 2, AccountStartDate),
            new("221", "長期借入金", "負債", 3, AccountStartDate),
            new("222", "リース債務", "負債", 3, AccountStartDate),

            // 純資産の部
            new("3", "純資産", "純資産", 1, AccountStartDate),
            new("31", "資本金", "純資産", 2, AccountStartDate),
            new("32", "資本剰余金", "純資産", 2, AccountStartDate),
            new("33", "利益剰余金", "純資産", 2, AccountStartDate),

            // 収益の部
            new("4", "収益", "収益", 1, AccountStartDate),
            new("41", "売上高", "収益", 2, AccountStartDate),
            new("42", "営業外収益", "収益", 2, AccountStartDate),
            new("43", "特別利益", "収益", 2, AccountStartDate),

            // 費用の部
            new("5", "費用", "費用", 1, AccountStartDate),
            new("51", "売上原価", "費用", 2, AccountStartDate),
            new("52", "販売費及び一般管理費", "費用", 2, AccountStartDate),
            new("53", "営業外費用", "費用", 2, AccountStartDate),
            new("54", "特別損失", "費用", 2, AccountStartDate),
            new("55", "法人税等", "費用", 2, AccountStartDate),
            new("56", "当期純利益", "費用", 2, AccountStartDate)
        };
    }

    #endregion

    #region 勘定科目構成マスタ

    /// <summary>
    /// 勘定科目構成マスタデータ（階層パス）
    /// チルダ(~)連結による階層パスを使用
    /// </summary>
    public static List<AccountStructureData> GetAccountStructures()
    {
        return new List<AccountStructureData>
        {
            // 資産の部
            new("1", "1"),
            new("11", "1~11"),
            new("111", "1~11~111"),
            new("112", "1~11~112"),
            new("113", "1~11~113"),
            new("114", "1~11~114"),
            new("115", "1~11~115"),
            new("12", "1~12"),
            new("121", "1~12~121"),
            new("1211", "1~12~121~1211"),
            new("1212", "1~12~121~1212"),
            new("1213", "1~12~121~1213"),
            new("1214", "1~12~121~1214"),
            new("1215", "1~12~121~1215"),
            new("122", "1~12~122"),
            new("123", "1~12~123"),

            // 負債の部
            new("2", "2"),
            new("21", "2~21"),
            new("211", "2~21~211"),
            new("212", "2~21~212"),
            new("213", "2~21~213"),
            new("214", "2~21~214"),
            new("215", "2~21~215"),
            new("22", "2~22"),
            new("221", "2~22~221"),
            new("222", "2~22~222"),

            // 純資産の部
            new("3", "3"),
            new("31", "3~31"),
            new("32", "3~32"),
            new("33", "3~33"),

            // 収益の部
            new("4", "4"),
            new("41", "4~41"),
            new("42", "4~42"),
            new("43", "4~43"),

            // 費用の部
            new("5", "5"),
            new("51", "5~51"),
            new("52", "5~52"),
            new("53", "5~53"),
            new("54", "5~54"),
            new("55", "5~55"),
            new("56", "5~56")
        };
    }

    #endregion

    #region 会計期間マスタ

    /// <summary>
    /// 会計期間マスタデータ（2期間）
    /// </summary>
    public static List<PeriodData> GetPeriods()
    {
        return new List<PeriodData>
        {
            new(2021, "令和3年度", new DateOnly(2021, 4, 1), new DateOnly(2022, 3, 31), true),
            new(2022, "令和4年度", new DateOnly(2022, 4, 1), new DateOnly(2023, 3, 31), true)
        };
    }

    #endregion

    #region 仕訳データ

    /// <summary>
    /// 令和3年度期末仕訳
    /// </summary>
    public static List<JournalData> GetFY2021Journals()
    {
        return new List<JournalData>
        {
            new(new DateOnly(2022, 3, 31), 2021, "令和3年度期末仕訳")
        };
    }

    /// <summary>
    /// 令和4年度期末仕訳
    /// </summary>
    public static List<JournalData> GetFY2022Journals()
    {
        return new List<JournalData>
        {
            new(new DateOnly(2023, 3, 31), 2022, "令和4年度期末仕訳")
        };
    }

    #endregion

    #region 仕訳明細データ

    /// <summary>
    /// 令和3年度仕訳明細（単位：千円）
    /// D社の貸借対照表・損益計算書データより
    /// </summary>
    public static List<JournalEntryData> GetFY2021Entries()
    {
        return new List<JournalEntryData>
        {
            // 資産の部（借方）
            new("111", 593_256m, 0m, "現金預金"),
            new("112", 1_085_840m, 0m, "売掛金・受取手形"),
            new("114", 948_537m, 0m, "棚卸資産"),
            new("115", 48_560m, 0m, "その他流動資産"),
            new("1211", 64_524m, 0m, "建物及び構築物"),
            new("122", 37_492m, 0m, "無形固定資産"),
            new("123", 84_957m, 0m, "投資その他の資産"),

            // 負債の部（貸方）
            new("211", 0m, 191_034m, "買掛金"),
            new("212", 0m, 120_000m, "短期借入金"),
            new("213", 0m, 197_262m, "未払金"),
            new("214", 0m, 250_114m, "未払法人税等"),
            new("215", 0m, 92_984m, "その他流動負債"),
            new("221", 0m, 22_500m, "長期借入金"),

            // 純資産の部（貸方）
            new("31", 0m, 100_000m, "資本金"),
            new("33", 0m, 1_889_272m, "利益剰余金"),

            // 収益の部（損益計算書・貸方）
            new("41", 0m, 5_796_105m, "売上高"),
            new("42", 0m, 368m, "営業外収益"),

            // 費用の部（損益計算書・借方）
            new("51", 2_185_856m, 0m, "売上原価"),
            new("52", 2_625_222m, 0m, "販売費及び一般管理費"),
            new("53", 2_676m, 0m, "営業外費用"),
            new("55", 331_059m, 0m, "法人税等")
        };
    }

    /// <summary>
    /// 令和4年度仕訳明細（単位：千円）
    /// D社の貸借対照表・損益計算書データより
    /// </summary>
    public static List<JournalEntryData> GetFY2022Entries()
    {
        return new List<JournalEntryData>
        {
            // 資産の部（借方）
            new("111", 1_133_270m, 0m, "現金預金"),
            new("112", 864_915m, 0m, "売掛金・受取手形"),
            new("114", 740_810m, 0m, "棚卸資産"),
            new("115", 38_550m, 0m, "その他流動資産"),
            new("1211", 63_256m, 0m, "建物及び構築物"),
            new("122", 34_683m, 0m, "無形固定資産"),
            new("123", 99_415m, 0m, "投資その他の資産"),

            // 負債の部（貸方）
            new("211", 0m, 197_162m, "買掛金"),
            new("212", 0m, 70_000m, "短期借入金"),
            new("213", 0m, 104_341m, "未払金"),
            new("214", 0m, 184_887m, "未払法人税等"),
            new("215", 0m, 84_123m, "その他流動負債"),
            new("221", 0m, 24_360m, "長期借入金"),
            new("222", 0m, 2_793m, "リース債務"),

            // 純資産の部（貸方）
            new("31", 0m, 100_000m, "資本金"),
            new("33", 0m, 2_207_233m, "利益剰余金"),

            // 収益の部（損益計算書・貸方）
            new("41", 0m, 4_547_908m, "売上高"),
            new("42", 0m, 11_608m, "営業外収益"),

            // 費用の部（損益計算書・借方）
            new("51", 1_743_821m, 0m, "売上原価"),
            new("52", 2_277_050m, 0m, "販売費及び一般管理費"),
            new("53", 1_613m, 0m, "営業外費用"),
            new("55", 169_072m, 0m, "法人税等")
        };
    }

    #endregion

    #region 日次勘定科目残高データ

    /// <summary>
    /// 令和3年度期末日次残高（単位：千円）
    /// </summary>
    public static List<DailyBalanceData> GetFY2021DailyBalances()
    {
        var balanceDate = new DateOnly(2022, 3, 31);
        return new List<DailyBalanceData>
        {
            // 流動資産
            new("111", balanceDate, 593_256m, 0m),
            new("112", balanceDate, 1_085_840m, 0m),
            new("114", balanceDate, 948_537m, 0m),
            new("115", balanceDate, 48_560m, 0m),
            // 固定資産
            new("1211", balanceDate, 64_524m, 0m),
            new("122", balanceDate, 37_492m, 0m),
            new("123", balanceDate, 84_957m, 0m),
            // 流動負債
            new("211", balanceDate, 0m, 191_034m),
            new("212", balanceDate, 0m, 120_000m),
            new("213", balanceDate, 0m, 197_262m),
            new("214", balanceDate, 0m, 250_114m),
            new("215", balanceDate, 0m, 92_984m),
            // 固定負債
            new("221", balanceDate, 0m, 22_500m)
        };
    }

    /// <summary>
    /// 令和4年度期末日次残高（単位：千円）
    /// </summary>
    public static List<DailyBalanceData> GetFY2022DailyBalances()
    {
        var balanceDate = new DateOnly(2023, 3, 31);
        return new List<DailyBalanceData>
        {
            // 流動資産
            new("111", balanceDate, 1_133_270m, 0m),
            new("112", balanceDate, 864_915m, 0m),
            new("114", balanceDate, 740_810m, 0m),
            new("115", balanceDate, 38_550m, 0m),
            // 固定資産
            new("1211", balanceDate, 63_256m, 0m),
            new("122", balanceDate, 34_683m, 0m),
            new("123", balanceDate, 99_415m, 0m),
            // 流動負債
            new("211", balanceDate, 0m, 197_162m),
            new("212", balanceDate, 0m, 70_000m),
            new("213", balanceDate, 0m, 104_341m),
            new("214", balanceDate, 0m, 184_887m),
            new("215", balanceDate, 0m, 84_123m),
            // 固定負債
            new("221", balanceDate, 0m, 24_360m),
            new("222", balanceDate, 0m, 2_793m)
        };
    }

    #endregion

    #region 月次勘定科目残高データ

    /// <summary>
    /// 令和3年度期末月次残高（3月=会計年度第12月）（単位：千円）
    /// </summary>
    public static List<MonthlyBalanceData> GetFY2021MonthlyBalances()
    {
        return new List<MonthlyBalanceData>
        {
            // 流動資産（集計）
            new("11", 2021, 12, 0m, 2_676_193m, 0m, 2_676_193m),  // 流動資産合計
            new("111", 2021, 12, 0m, 593_256m, 0m, 593_256m),
            new("112", 2021, 12, 0m, 1_085_840m, 0m, 1_085_840m),
            new("114", 2021, 12, 0m, 948_537m, 0m, 948_537m),
            new("115", 2021, 12, 0m, 48_560m, 0m, 48_560m),
            // 固定資産（集計）
            new("12", 2021, 12, 0m, 186_973m, 0m, 186_973m),  // 固定資産合計
            new("121", 2021, 12, 0m, 64_524m, 0m, 64_524m),  // 有形固定資産合計
            new("1211", 2021, 12, 0m, 64_524m, 0m, 64_524m),
            new("122", 2021, 12, 0m, 37_492m, 0m, 37_492m),
            new("123", 2021, 12, 0m, 84_957m, 0m, 84_957m),
            // 流動負債（集計）
            new("21", 2021, 12, 0m, 0m, 851_394m, 851_394m),  // 流動負債合計
            new("211", 2021, 12, 0m, 0m, 191_034m, 191_034m),
            new("212", 2021, 12, 0m, 0m, 120_000m, 120_000m),
            new("213", 2021, 12, 0m, 0m, 197_262m, 197_262m),
            new("214", 2021, 12, 0m, 0m, 250_114m, 250_114m),
            new("215", 2021, 12, 0m, 0m, 92_984m, 92_984m),
            // 固定負債（集計）
            new("22", 2021, 12, 0m, 0m, 22_500m, 22_500m),  // 固定負債合計
            new("221", 2021, 12, 0m, 0m, 22_500m, 22_500m),
            // 純資産
            new("31", 2021, 12, 0m, 0m, 100_000m, 100_000m),  // 資本金
            new("33", 2021, 12, 0m, 0m, 1_889_272m, 1_889_272m),  // 利益剰余金
            // 損益計算書項目（収益は貸方、費用は借方）
            new("41", 2021, 12, 0m, 0m, 5_796_105m, 5_796_105m),  // 売上高
            new("42", 2021, 12, 0m, 0m, 368m, 368m),  // 営業外収益
            new("51", 2021, 12, 0m, 2_185_856m, 0m, 2_185_856m),  // 売上原価
            new("52", 2021, 12, 0m, 2_625_222m, 0m, 2_625_222m),  // 販売費及び一般管理費
            new("53", 2021, 12, 0m, 2_676m, 0m, 2_676m),  // 営業外費用
            new("55", 2021, 12, 0m, 331_059m, 0m, 331_059m)  // 法人税等
        };
    }

    /// <summary>
    /// 令和4年度期末月次残高（3月=会計年度第12月）（単位：千円）
    /// </summary>
    public static List<MonthlyBalanceData> GetFY2022MonthlyBalances()
    {
        return new List<MonthlyBalanceData>
        {
            // 流動資産（集計）
            new("11", 2022, 12, 0m, 2_777_545m, 0m, 2_777_545m),  // 流動資産合計
            new("111", 2022, 12, 0m, 1_133_270m, 0m, 1_133_270m),
            new("112", 2022, 12, 0m, 864_915m, 0m, 864_915m),
            new("114", 2022, 12, 0m, 740_810m, 0m, 740_810m),
            new("115", 2022, 12, 0m, 38_550m, 0m, 38_550m),
            // 固定資産（集計）
            new("12", 2022, 12, 0m, 197_354m, 0m, 197_354m),  // 固定資産合計
            new("121", 2022, 12, 0m, 63_256m, 0m, 63_256m),  // 有形固定資産合計
            new("1211", 2022, 12, 0m, 63_256m, 0m, 63_256m),
            new("122", 2022, 12, 0m, 34_683m, 0m, 34_683m),
            new("123", 2022, 12, 0m, 99_415m, 0m, 99_415m),
            // 流動負債（集計）
            new("21", 2022, 12, 0m, 0m, 640_513m, 640_513m),  // 流動負債合計
            new("211", 2022, 12, 0m, 0m, 197_162m, 197_162m),
            new("212", 2022, 12, 0m, 0m, 70_000m, 70_000m),
            new("213", 2022, 12, 0m, 0m, 104_341m, 104_341m),
            new("214", 2022, 12, 0m, 0m, 184_887m, 184_887m),
            new("215", 2022, 12, 0m, 0m, 84_123m, 84_123m),
            // 固定負債（集計）
            new("22", 2022, 12, 0m, 0m, 27_153m, 27_153m),  // 固定負債合計
            new("221", 2022, 12, 0m, 0m, 24_360m, 24_360m),
            new("222", 2022, 12, 0m, 0m, 2_793m, 2_793m),
            // 純資産
            new("31", 2022, 12, 0m, 0m, 100_000m, 100_000m),  // 資本金
            new("33", 2022, 12, 0m, 0m, 2_207_233m, 2_207_233m),  // 利益剰余金
            // 損益計算書項目（収益は貸方、費用は借方）
            new("41", 2022, 12, 0m, 0m, 4_547_908m, 4_547_908m),  // 売上高
            new("42", 2022, 12, 0m, 0m, 11_608m, 11_608m),  // 営業外収益
            new("51", 2022, 12, 0m, 1_743_821m, 0m, 1_743_821m),  // 売上原価
            new("52", 2022, 12, 0m, 2_277_050m, 0m, 2_277_050m),  // 販売費及び一般管理費
            new("53", 2022, 12, 0m, 1_613m, 0m, 1_613m),  // 営業外費用
            new("55", 2022, 12, 0m, 169_072m, 0m, 169_072m)  // 法人税等
        };
    }

    #endregion
}
