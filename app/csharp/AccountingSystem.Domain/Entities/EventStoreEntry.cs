namespace AccountingSystem.Domain.Entities;

/// <summary>
/// イベントストアエントリ
/// </summary>
public class EventStoreEntry
{
    /// <summary>
    /// イベントID（グローバルシーケンス）
    /// </summary>
    public long EventId { get; set; }

    /// <summary>
    /// 集約ID
    /// </summary>
    public string AggregateId { get; set; } = string.Empty;

    /// <summary>
    /// 集約種別
    /// </summary>
    public string AggregateType { get; set; } = string.Empty;

    /// <summary>
    /// イベント種別
    /// </summary>
    public string EventType { get; set; } = string.Empty;

    /// <summary>
    /// イベントバージョン
    /// </summary>
    public int EventVersion { get; set; } = 1;

    /// <summary>
    /// イベントデータ（JSON）
    /// </summary>
    public string EventData { get; set; } = string.Empty;

    /// <summary>
    /// 発生日時
    /// </summary>
    public DateTime OccurredAt { get; set; }

    /// <summary>
    /// ユーザーID
    /// </summary>
    public string? UserId { get; set; }

    /// <summary>
    /// 相関ID
    /// </summary>
    public string? CorrelationId { get; set; }

    /// <summary>
    /// 因果ID
    /// </summary>
    public string? CausationId { get; set; }

    /// <summary>
    /// シーケンス番号（Aggregate内の順序）
    /// </summary>
    public int SequenceNumber { get; set; }
}
