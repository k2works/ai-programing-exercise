namespace AccountingSystem.Domain.Entities;

/// <summary>
/// 仕訳明細エンティティ
/// </summary>
public record JournalDetail
{
    /// <summary>仕訳伝票番号</summary>
    public required string JournalNo { get; init; }

    /// <summary>仕訳行番号</summary>
    public required int LineNumber { get; init; }

    /// <summary>行摘要</summary>
    public required string Description { get; init; }

    /// <summary>作成日時</summary>
    public DateTime CreatedAt { get; init; }

    /// <summary>更新日時</summary>
    public DateTime UpdatedAt { get; init; }

    /// <summary>仕訳貸借明細リスト</summary>
    public List<JournalDetailItem> Items { get; init; } = new();
}
