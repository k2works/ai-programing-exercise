namespace FinancialAccounting.Domain.Entities;

/// <summary>
/// 勘定科目エンティティ
/// </summary>
public class Account
{
    public int AccountId { get; set; }
    public string AccountCode { get; set; } = string.Empty;
    public string AccountName { get; set; } = string.Empty;
    public string? AccountNameKana { get; set; }
    public string AccountType { get; set; } = string.Empty;  // 資産, 負債, 純資産, 収益, 費用
    public bool IsSummaryAccount { get; set; }
    public string BsPlType { get; set; } = string.Empty;     // B: 貸借対照表, P: 損益計算書
    public string TransactionElementType { get; set; } = string.Empty;
    public string? ExpenseType { get; set; }
    public int DisplayOrder { get; set; }
    public bool IsAggregationTarget { get; set; }
    public string? TaxCode { get; set; }
    public decimal Balance { get; set; }
    public DateTime CreatedAt { get; set; }
    public DateTime UpdatedAt { get; set; }
}
