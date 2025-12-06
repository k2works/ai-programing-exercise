namespace AccountingSystem.Infrastructure.Migrations

open FluentMigrator

/// <summary>
/// スナップショットストアテーブルの作成
/// イベントソーシングの最適化のためのスナップショット機能
/// </summary>
[<Migration(20250121019L)>]
type Migration_20250121_019_CreateSnapshotStore() =
    inherit Migration()

    override this.Up() =
        // スナップショットストアテーブルの作成
        this.Execute.Sql("""
            CREATE TABLE IF NOT EXISTS snapshot_store (
                -- 主キー
                snapshot_id BIGSERIAL PRIMARY KEY,

                -- Aggregate 識別子
                aggregate_id VARCHAR(100) NOT NULL,
                aggregate_type VARCHAR(50) NOT NULL,

                -- スナップショットメタデータ
                version INTEGER NOT NULL,

                -- スナップショットデータ（JSONB形式）
                snapshot_data JSONB NOT NULL,

                -- メタデータ
                created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,

                -- 一意性制約（同一 Aggregate の最新スナップショットのみ保持）
                CONSTRAINT uk_snapshot_aggregate
                    UNIQUE (aggregate_id, aggregate_type)
            );

            -- テーブルコメント
            COMMENT ON TABLE snapshot_store IS 'スナップショットストア - イベント再生の最適化用';
            COMMENT ON COLUMN snapshot_store.snapshot_id IS 'スナップショットID（自動採番）';
            COMMENT ON COLUMN snapshot_store.aggregate_id IS 'Aggregate識別子';
            COMMENT ON COLUMN snapshot_store.aggregate_type IS 'Aggregateタイプ';
            COMMENT ON COLUMN snapshot_store.version IS 'スナップショット時点のバージョン';
            COMMENT ON COLUMN snapshot_store.snapshot_data IS 'Aggregate状態（JSONB）';
            COMMENT ON COLUMN snapshot_store.created_at IS 'スナップショット作成日時';
        """)

        // インデックスの作成
        this.Execute.Sql("""
            -- Aggregate ID + タイプのインデックス（主要な検索パターン）
            CREATE INDEX IF NOT EXISTS idx_snapshot_store_aggregate
                ON snapshot_store(aggregate_id, aggregate_type);

            -- 作成日時のインデックス（古いスナップショット削除用）
            CREATE INDEX IF NOT EXISTS idx_snapshot_store_created_at
                ON snapshot_store(created_at);
        """)

    override this.Down() =
        this.Execute.Sql("""
            DROP INDEX IF EXISTS idx_snapshot_store_created_at;
            DROP INDEX IF EXISTS idx_snapshot_store_aggregate;
            DROP TABLE IF EXISTS snapshot_store;
        """)
