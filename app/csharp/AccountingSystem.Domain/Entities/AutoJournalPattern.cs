namespace AccountingSystem.Domain.Entities;

/// <summary>
/// 自動仕訳パターンエンティティ
/// 自動仕訳の生成ルールを定義するパターンマスタ
/// </summary>
public record AutoJournalPattern
{
    /// <summary>自動仕訳パターンID</summary>
    public long AutoJournalPatternId { get; init; }

    /// <summary>パターンコード</summary>
    public required string PatternCode { get; init; }

    /// <summary>パターン名</summary>
    public required string PatternName { get; init; }

    /// <summary>ソーステーブル名</summary>
    public required string SourceTableName { get; init; }

    /// <summary>説明</summary>
    public string? Description { get; init; }

    /// <summary>有効フラグ</summary>
    public required bool IsActive { get; init; }

    /// <summary>作成日時</summary>
    public DateTime CreatedAt { get; init; }

    /// <summary>更新日時</summary>
    public DateTime UpdatedAt { get; init; }

    /// <summary>自動仕訳パターン明細リスト</summary>
    public List<AutoJournalPatternDetail> Details { get; init; } = new();
}
