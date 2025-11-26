using AccountingSystem.Domain.Models;

namespace AccountingSystem.Infrastructure.Web.Dtos;

/// <summary>
/// 損益計算書レスポンス DTO
/// </summary>
public class IncomeStatementResponse
{
    /// <summary>
    /// 開始日
    /// </summary>
    public DateOnly FromDate { get; set; }

    /// <summary>
    /// 終了日
    /// </summary>
    public DateOnly ToDate { get; set; }

    /// <summary>
    /// 収益
    /// </summary>
    public List<IncomeStatementItemResponse> Revenues { get; set; } = new();

    /// <summary>
    /// 費用
    /// </summary>
    public List<IncomeStatementItemResponse> Expenses { get; set; } = new();

    /// <summary>
    /// 収益合計
    /// </summary>
    public decimal TotalRevenues { get; set; }

    /// <summary>
    /// 費用合計
    /// </summary>
    public decimal TotalExpenses { get; set; }

    /// <summary>
    /// 売上総利益
    /// </summary>
    public decimal GrossProfit { get; set; }

    /// <summary>
    /// 営業利益
    /// </summary>
    public decimal OperatingIncome { get; set; }

    /// <summary>
    /// 当期純利益
    /// </summary>
    public decimal NetIncome { get; set; }

    /// <summary>
    /// Domain Model からの変換
    /// </summary>
    public static IncomeStatementResponse From(IncomeStatement incomeStatement)
    {
        return new IncomeStatementResponse
        {
            FromDate = incomeStatement.FromDate,
            ToDate = incomeStatement.ToDate,
            Revenues = incomeStatement.Revenues
                .Select(IncomeStatementItemResponse.From)
                .ToList(),
            Expenses = incomeStatement.Expenses
                .Select(IncomeStatementItemResponse.From)
                .ToList(),
            TotalRevenues = incomeStatement.TotalRevenues,
            TotalExpenses = incomeStatement.TotalExpenses,
            GrossProfit = incomeStatement.GrossProfit,
            OperatingIncome = incomeStatement.OperatingIncome,
            NetIncome = incomeStatement.NetIncome
        };
    }
}

/// <summary>
/// 損益計算書項目レスポンス DTO
/// </summary>
public class IncomeStatementItemResponse
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
    /// 金額
    /// </summary>
    public decimal Amount { get; set; }

    /// <summary>
    /// 対売上比率（%）
    /// </summary>
    public decimal Percentage { get; set; }

    /// <summary>
    /// Domain Model からの変換
    /// </summary>
    public static IncomeStatementItemResponse From(IncomeStatementItem item)
    {
        return new IncomeStatementItemResponse
        {
            AccountCode = item.AccountCode,
            AccountName = item.AccountName,
            Amount = item.Amount,
            Percentage = item.Percentage
        };
    }
}
