using ManagementAccounting.Application.Ports.Out;
using ManagementAccounting.Domain.Entities;

namespace ManagementAccounting.Infrastructure.Adapters.External;

/// <summary>
/// 財務会計サービスクライアント（腐敗防止層）
///
/// 財務会計コンテキストのデータモデルの変更から
/// 管理会計コンテキストを保護する
/// </summary>
public class FinancialAccountingClient : IFinancialAccountingClient
{
    private readonly IFinancialAccountingApi _api;

    public FinancialAccountingClient(IFinancialAccountingApi api)
    {
        _api = api;
    }

    /// <summary>
    /// 財務会計サービスからデータを取得し、管理会計ドメインモデルに変換
    /// </summary>
    public async Task<FinancialData> FetchFinancialDataByFiscalYearAsync(int fiscalYear)
    {
        var journals = await _api.GetJournalsByFiscalYearAsync(fiscalYear);

        // 全仕訳明細をフラット化
        var entries = journals.SelectMany(j => j.Entries).ToList();

        // 管理会計コンテキストのドメインモデルに変換
        return ConvertToFinancialData(fiscalYear, entries);
    }

    /// <summary>
    /// 財務会計のデータモデルを管理会計のドメインモデルに変換
    ///
    /// この変換ロジックにより、財務会計のスキーマ変更の影響を局所化
    /// </summary>
    private FinancialData ConvertToFinancialData(int fiscalYear, List<JournalEntryDto> entries)
    {
        // 売上高（41 から始まる勘定科目）
        var sales = SumByAccountPrefix(entries, "41", "credit");

        // 営業利益（簡易計算: 収益 - 費用）
        var revenue = SumByAccountPrefix(entries, "41", "credit");
        var costOfSales = SumByAccountPrefix(entries, "51", "debit");
        var sellingExpenses = SumByAccountPrefix(entries, "52", "debit");
        var operatingProfit = revenue - costOfSales - sellingExpenses;

        // 総資産（1 から始まる勘定科目）
        var totalAssets = SumByAccountPrefix(entries, "1", "debit");

        // 有形固定資産（121 から始まる勘定科目）
        var tangibleFixedAssets = SumByAccountPrefix(entries, "121", "debit");

        // 流動資産（11 から始まる勘定科目）
        var currentAssets = SumByAccountPrefix(entries, "11", "debit");

        // 流動負債（21 から始まる勘定科目）
        var currentLiabilities = SumByAccountPrefix(entries, "21", "credit");

        // 当座資産（現金預金 + 売掛金）
        var quickAssets = SumByAccountCode(entries, "111", "debit")   // 現金預金
                        + SumByAccountCode(entries, "112", "debit");  // 売掛金

        // 自己資本（3 から始まる勘定科目）
        var equity = SumByAccountPrefix(entries, "3", "credit");

        return new FinancialData(
            FiscalYear: fiscalYear,
            Sales: sales,
            OperatingProfit: operatingProfit,
            TotalAssets: totalAssets,
            TangibleFixedAssets: tangibleFixedAssets,
            CurrentAssets: currentAssets,
            CurrentLiabilities: currentLiabilities,
            QuickAssets: quickAssets,
            Equity: equity
        );
    }

    private decimal SumByAccountPrefix(List<JournalEntryDto> entries, string prefix, string side)
    {
        return entries
            .Where(e => e.AccountCode.StartsWith(prefix))
            .Sum(e => side == "debit" ? e.DebitAmount : e.CreditAmount);
    }

    private decimal SumByAccountCode(List<JournalEntryDto> entries, string accountCode, string side)
    {
        return entries
            .Where(e => e.AccountCode == accountCode)
            .Sum(e => side == "debit" ? e.DebitAmount : e.CreditAmount);
    }
}
