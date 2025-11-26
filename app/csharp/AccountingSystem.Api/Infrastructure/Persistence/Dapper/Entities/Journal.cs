namespace AccountingSystem.Infrastructure.Persistence.Dapper.Entities;

/// <summary>
/// 仕訳エンティティ（ヘッダー）
/// </summary>
public record Journal
{
    /// <summary>仕訳伝票番号</summary>
    public required string JournalNo { get; init; }

    /// <summary>起票日</summary>
    public required DateOnly JournalDate { get; init; }

    /// <summary>入力日</summary>
    public required DateOnly InputDate { get; init; }

    /// <summary>決算仕訳フラグ（0=通常、1=決算）</summary>
    public required int SettlementFlag { get; init; }

    /// <summary>単振フラグ（0=複合、1=単一）</summary>
    public required int SingleEntryFlag { get; init; }

    /// <summary>仕訳伝票区分</summary>
    public required int JournalType { get; init; }

    /// <summary>定期計上フラグ</summary>
    public required int RecurringFlag { get; init; }

    /// <summary>社員コード</summary>
    public string? EmployeeCode { get; init; }

    /// <summary>部門コード</summary>
    public string? DepartmentCode { get; init; }

    /// <summary>赤伝フラグ（0=通常、1=赤伝）</summary>
    public required int RedSlipFlag { get; init; }

    /// <summary>赤黒伝票番号</summary>
    public string? RedBlackVoucherNo { get; init; }

    /// <summary>作成日時</summary>
    public DateTime CreatedAt { get; init; }

    /// <summary>更新日時</summary>
    public DateTime UpdatedAt { get; init; }

    /// <summary>仕訳明細リスト</summary>
    public List<JournalDetail> Details { get; init; } = new();
}
