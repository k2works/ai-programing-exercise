using AccountingSystem.Domain.Models;

namespace AccountingSystem.Infrastructure.Web.Dtos;

/// <summary>
/// 貸借対照表レスポンス DTO
/// </summary>
public class BalanceSheetResponse
{
    /// <summary>
    /// 基準日
    /// </summary>
    public DateOnly AsOfDate { get; set; }

    /// <summary>
    /// 資産
    /// </summary>
    public List<BalanceSheetItemResponse> Assets { get; set; } = new();

    /// <summary>
    /// 負債
    /// </summary>
    public List<BalanceSheetItemResponse> Liabilities { get; set; } = new();

    /// <summary>
    /// 純資産
    /// </summary>
    public List<BalanceSheetItemResponse> Equity { get; set; } = new();

    /// <summary>
    /// 資産合計
    /// </summary>
    public decimal TotalAssets { get; set; }

    /// <summary>
    /// 負債合計
    /// </summary>
    public decimal TotalLiabilities { get; set; }

    /// <summary>
    /// 純資産合計
    /// </summary>
    public decimal TotalEquity { get; set; }

    /// <summary>
    /// 負債・純資産合計
    /// </summary>
    public decimal TotalLiabilitiesAndEquity { get; set; }

    /// <summary>
    /// Domain Model からの変換
    /// </summary>
    public static BalanceSheetResponse From(BalanceSheet balanceSheet)
    {
        return new BalanceSheetResponse
        {
            AsOfDate = balanceSheet.AsOfDate,
            Assets = balanceSheet.Assets
                .Select(BalanceSheetItemResponse.From)
                .ToList(),
            Liabilities = balanceSheet.Liabilities
                .Select(BalanceSheetItemResponse.From)
                .ToList(),
            Equity = balanceSheet.Equity
                .Select(BalanceSheetItemResponse.From)
                .ToList(),
            TotalAssets = balanceSheet.TotalAssets,
            TotalLiabilities = balanceSheet.TotalLiabilities,
            TotalEquity = balanceSheet.TotalEquity,
            TotalLiabilitiesAndEquity = balanceSheet.TotalLiabilitiesAndEquity
        };
    }
}

/// <summary>
/// 貸借対照表項目レスポンス DTO
/// </summary>
public class BalanceSheetItemResponse
{
    /// <summary>
    /// 勘定科目コード
    /// </summary>
    public string AccountCode { get; set; } = string.Empty;

    /// <summary>
    /// 勘定科目名
    /// </summary>
    public string AccountName { get; set; } = string.Empty;

    /// <summary>
    /// 残高
    /// </summary>
    public decimal Balance { get; set; }

    /// <summary>
    /// 構成比率（%）
    /// </summary>
    public decimal Percentage { get; set; }

    /// <summary>
    /// Domain Model からの変換
    /// </summary>
    public static BalanceSheetItemResponse From(BalanceSheetItem item)
    {
        return new BalanceSheetItemResponse
        {
            AccountCode = item.AccountCode,
            AccountName = item.AccountName,
            Balance = item.Balance,
            Percentage = item.Percentage
        };
    }
}
