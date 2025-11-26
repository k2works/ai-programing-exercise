namespace AccountingSystem.Domain.Entities;

/// <summary>
/// スナップショットエンティティ
/// イベント再生の最適化用
/// </summary>
public class SnapshotEntry
{
    /// <summary>
    /// スナップショットID
    /// </summary>
    public long SnapshotId { get; set; }

    /// <summary>
    /// 集約ID
    /// </summary>
    public string AggregateId { get; set; } = string.Empty;

    /// <summary>
    /// 集約種別
    /// </summary>
    public string AggregateType { get; set; } = string.Empty;

    /// <summary>
    /// スナップショットデータ（JSON）
    /// </summary>
    public string SnapshotData { get; set; } = string.Empty;

    /// <summary>
    /// シーケンス番号（スナップショット時点）
    /// </summary>
    public int SequenceNumber { get; set; }

    /// <summary>
    /// 作成日時
    /// </summary>
    public DateTime CreatedAt { get; set; }
}
