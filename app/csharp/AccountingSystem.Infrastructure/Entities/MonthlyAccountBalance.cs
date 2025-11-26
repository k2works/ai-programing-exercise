namespace AccountingSystem.Infrastructure.Entities;

/// <summary>
/// 月次勘定科目残高エンティティ
/// </summary>
public record MonthlyAccountBalance
{
    /// <summary>決算期（例：20250000）</summary>
    public required int FiscalYear { get; init; }

    /// <summary>月度（1～12）</summary>
    public required int Month { get; init; }

    /// <summary>勘定科目コード</summary>
    public required string AccountCode { get; init; }

    /// <summary>補助科目コード</summary>
    public required string SubAccountCode { get; init; }

    /// <summary>部門コード</summary>
    public required string DepartmentCode { get; init; }

    /// <summary>プロジェクトコード</summary>
    public required string ProjectCode { get; init; }

    /// <summary>決算仕訳フラグ（0=通常、1=決算）</summary>
    public required int SettlementFlag { get; init; }

    /// <summary>月初残高</summary>
    public required decimal OpeningBalance { get; init; }

    /// <summary>借方金額</summary>
    public required decimal DebitAmount { get; init; }

    /// <summary>貸方金額</summary>
    public required decimal CreditAmount { get; init; }

    /// <summary>月末残高</summary>
    public required decimal ClosingBalance { get; init; }

    /// <summary>作成日時</summary>
    public DateTime CreatedAt { get; init; }

    /// <summary>更新日時</summary>
    public DateTime UpdatedAt { get; init; }

    /// <summary>
    /// 月中の変動額を計算（借方金額 - 貸方金額）
    /// </summary>
    public decimal GetMonthlyChange() => DebitAmount - CreditAmount;

    /// <summary>
    /// 月末残高が正しく計算されているかを検証
    /// </summary>
    public bool IsBalanceValid() => ClosingBalance == OpeningBalance + DebitAmount - CreditAmount;
}
