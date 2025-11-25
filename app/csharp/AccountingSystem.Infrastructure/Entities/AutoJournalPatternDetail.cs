namespace AccountingSystem.Infrastructure.Entities;

/// <summary>
/// 自動仕訳パターン明細エンティティ
/// 自動仕訳パターンの借方・貸方明細
/// </summary>
public record AutoJournalPatternDetail
{
    /// <summary>自動仕訳パターン明細ID</summary>
    public long AutoJournalPatternDetailId { get; init; }

    /// <summary>自動仕訳パターンID</summary>
    public required long AutoJournalPatternId { get; init; }

    /// <summary>行番号</summary>
    public required int LineNumber { get; init; }

    /// <summary>貸借区分（D=借方、C=貸方）</summary>
    public required string DebitCreditFlag { get; init; }

    /// <summary>勘定科目コード</summary>
    public required string AccountCode { get; init; }

    /// <summary>金額算出式（例: 税込金額、税抜金額 * 0.1）</summary>
    public required string AmountExpression { get; init; }

    /// <summary>摘要テンプレート（例: 売上: {顧客名}）</summary>
    public string? DescriptionTemplate { get; init; }

    /// <summary>作成日時</summary>
    public DateTime CreatedAt { get; init; }

    /// <summary>更新日時</summary>
    public DateTime UpdatedAt { get; init; }
}
