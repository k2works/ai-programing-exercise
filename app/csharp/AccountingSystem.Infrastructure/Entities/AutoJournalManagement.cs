namespace AccountingSystem.Infrastructure.Entities;

/// <summary>
/// 自動仕訳管理エンティティ
/// 日付管理方式による自動仕訳の処理状況を管理
/// </summary>
public record AutoJournalManagement
{
    /// <summary>自動仕訳管理ID</summary>
    public long AutoJournalManagementId { get; init; }

    /// <summary>ソーステーブル名</summary>
    public required string SourceTableName { get; init; }

    /// <summary>最終処理日時（差分処理の基準）</summary>
    public required DateTime LastProcessedAt { get; init; }

    /// <summary>作成日時</summary>
    public DateTime CreatedAt { get; init; }

    /// <summary>更新日時</summary>
    public DateTime UpdatedAt { get; init; }
}
