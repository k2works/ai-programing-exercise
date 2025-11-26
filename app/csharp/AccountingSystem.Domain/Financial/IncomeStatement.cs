namespace AccountingSystem.Domain.Financial;

/// <summary>
/// 損益計算書（Income Statement / P/L）
/// </summary>
public record IncomeStatement
{
    /// <summary>
    /// 開始日
    /// </summary>
    public required DateOnly FromDate { get; init; }

    /// <summary>
    /// 終了日
    /// </summary>
    public required DateOnly ToDate { get; init; }

    /// <summary>
    /// 収益項目
    /// </summary>
    public required IReadOnlyList<IncomeStatementItem> Revenues { get; init; }

    /// <summary>
    /// 費用項目
    /// </summary>
    public required IReadOnlyList<IncomeStatementItem> Expenses { get; init; }

    /// <summary>
    /// 売上総利益（Gross Profit）
    /// </summary>
    public required decimal GrossProfit { get; init; }

    /// <summary>
    /// 営業利益（Operating Income）
    /// </summary>
    public required decimal OperatingIncome { get; init; }

    /// <summary>
    /// 当期純利益（Net Income）
    /// </summary>
    public required decimal NetIncome { get; init; }

    /// <summary>
    /// 総収益
    /// </summary>
    public required decimal TotalRevenues { get; init; }

    /// <summary>
    /// 総費用
    /// </summary>
    public required decimal TotalExpenses { get; init; }
}
