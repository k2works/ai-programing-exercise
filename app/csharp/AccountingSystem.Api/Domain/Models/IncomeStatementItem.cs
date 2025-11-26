namespace AccountingSystem.Domain.Models;

/// <summary>
/// 損益計算書の項目（勘定科目ごとの金額と構成比率）
/// </summary>
public record IncomeStatementItem
{
    /// <summary>
    /// 勘定科目コード
    /// </summary>
    public required string AccountCode { get; init; }

    /// <summary>
    /// 勘定科目名
    /// </summary>
    public required string AccountName { get; init; }

    /// <summary>
    /// 金額
    /// </summary>
    public required decimal Amount { get; init; }

    /// <summary>
    /// 対売上比率（%）
    /// </summary>
    public required decimal Percentage { get; init; }
}
