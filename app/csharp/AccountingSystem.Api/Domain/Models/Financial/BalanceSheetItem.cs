namespace AccountingSystem.Domain.Models.Financial;

/// <summary>
/// 貸借対照表の項目
/// </summary>
public record BalanceSheetItem
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
    /// 残高
    /// </summary>
    public required decimal Balance { get; init; }

    /// <summary>
    /// 構成比率（%）
    /// </summary>
    public required decimal Percentage { get; init; }
}
