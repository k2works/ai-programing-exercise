namespace AccountingSystem.Infrastructure.Persistence.Dapper.Entities;

/// <summary>
/// 自動仕訳実行ログエンティティ
/// 自動仕訳の実行履歴（監査証跡）
/// </summary>
public record AutoJournalExecutionLog
{
    /// <summary>自動仕訳実行ログID</summary>
    public long AutoJournalExecutionLogId { get; init; }

    /// <summary>自動仕訳パターンID</summary>
    public required long AutoJournalPatternId { get; init; }

    /// <summary>実行日時</summary>
    public required DateTime ExecutedAt { get; init; }

    /// <summary>処理件数</summary>
    public required int ProcessedCount { get; init; }

    /// <summary>生成件数</summary>
    public required int GeneratedCount { get; init; }

    /// <summary>ステータス（SUCCESS, ERROR, WARNING, RUNNING）</summary>
    public required string Status { get; init; }

    /// <summary>メッセージ</summary>
    public string? Message { get; init; }

    /// <summary>エラー詳細</summary>
    public string? ErrorDetail { get; init; }

    /// <summary>作成日時</summary>
    public DateTime CreatedAt { get; init; }
}
