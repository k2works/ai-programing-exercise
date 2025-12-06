namespace AccountingSystem.Infrastructure.Persistence.DAO

open System

/// <summary>
/// スナップショットストアエンティティ（Dapper マッピング用）
/// </summary>
[<CLIMutable>]
type SnapshotStoreEntity = {
    snapshot_id: int64
    aggregate_id: string
    aggregate_type: string
    version: int
    snapshot_data: string
    created_at: DateTime
}

module SnapshotStoreEntity =
    /// 空のエンティティを作成
    let empty = {
        snapshot_id = 0L
        aggregate_id = ""
        aggregate_type = ""
        version = 0
        snapshot_data = "{}"
        created_at = DateTime.UtcNow
    }

    /// 新規スナップショットエンティティを作成
    let create
        (aggregateId: string)
        (aggregateType: string)
        (version: int)
        (snapshotData: string)
        : SnapshotStoreEntity =
        {
            snapshot_id = 0L
            aggregate_id = aggregateId
            aggregate_type = aggregateType
            version = version
            snapshot_data = snapshotData
            created_at = DateTime.UtcNow
        }
