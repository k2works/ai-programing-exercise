namespace AccountingSystem.Infrastructure.Migrations

open FluentMigrator

/// <summary>
/// イベントストアテーブルの作成
/// イベントソーシングパターンの中核となるテーブル
/// </summary>
[<Migration(20250121017L)>]
type Migration_20250121_017_CreateEventStore() =
    inherit Migration()

    override this.Up() =
        // イベントストアテーブルの作成
        this.Execute.Sql("""
            CREATE TABLE IF NOT EXISTS event_store (
                -- 主キー（シーケンス番号）
                event_id BIGSERIAL PRIMARY KEY,

                -- Aggregate 識別子
                aggregate_id VARCHAR(100) NOT NULL,
                aggregate_type VARCHAR(50) NOT NULL,

                -- イベントメタデータ
                event_type VARCHAR(100) NOT NULL,
                event_version INTEGER NOT NULL DEFAULT 1,

                -- イベントデータ（JSONB形式）
                event_data JSONB NOT NULL,

                -- メタデータ
                occurred_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
                user_id VARCHAR(100),
                correlation_id VARCHAR(100),
                causation_id VARCHAR(100),

                -- 楽観的ロック（同時実行制御）
                sequence_number INTEGER NOT NULL,

                -- 一意性制約（同一 Aggregate の sequence_number は一意）
                CONSTRAINT uk_aggregate_sequence
                    UNIQUE (aggregate_id, sequence_number)
            );

            -- テーブルコメント
            COMMENT ON TABLE event_store IS 'イベントストア - イベントソーシングの中核テーブル';
            COMMENT ON COLUMN event_store.event_id IS 'イベントID（自動採番）';
            COMMENT ON COLUMN event_store.aggregate_id IS 'Aggregate識別子';
            COMMENT ON COLUMN event_store.aggregate_type IS 'Aggregateタイプ';
            COMMENT ON COLUMN event_store.event_type IS 'イベントタイプ';
            COMMENT ON COLUMN event_store.event_version IS 'イベントスキーマバージョン';
            COMMENT ON COLUMN event_store.event_data IS 'イベントデータ（JSONB）';
            COMMENT ON COLUMN event_store.occurred_at IS 'イベント発生日時';
            COMMENT ON COLUMN event_store.user_id IS '操作ユーザーID';
            COMMENT ON COLUMN event_store.correlation_id IS '相関ID（リクエスト追跡用）';
            COMMENT ON COLUMN event_store.causation_id IS '因果ID（イベントチェーン追跡用）';
            COMMENT ON COLUMN event_store.sequence_number IS 'シーケンス番号（楽観的ロック用）';
        """)

        // インデックスの作成
        this.Execute.Sql("""
            -- Aggregate ID + シーケンス番号のインデックス（主要な検索パターン）
            CREATE INDEX IF NOT EXISTS idx_event_store_aggregate_id
                ON event_store(aggregate_id, sequence_number);

            -- イベントタイプのインデックス（イベント種別での検索用）
            CREATE INDEX IF NOT EXISTS idx_event_store_event_type
                ON event_store(event_type);

            -- 発生日時のインデックス（時系列クエリ用）
            CREATE INDEX IF NOT EXISTS idx_event_store_occurred_at
                ON event_store(occurred_at);

            -- 相関IDのインデックス（リクエスト追跡用）
            CREATE INDEX IF NOT EXISTS idx_event_store_correlation_id
                ON event_store(correlation_id)
                WHERE correlation_id IS NOT NULL;

            -- JSONB用のGINインデックス（イベントデータ検索用）
            CREATE INDEX IF NOT EXISTS idx_event_store_event_data
                ON event_store USING GIN (event_data);

            -- Aggregate タイプ + 発生日時のインデックス（タイプ別時系列クエリ用）
            CREATE INDEX IF NOT EXISTS idx_event_store_aggregate_type_occurred_at
                ON event_store(aggregate_type, occurred_at);
        """)

    override this.Down() =
        this.Execute.Sql("""
            DROP INDEX IF EXISTS idx_event_store_aggregate_type_occurred_at;
            DROP INDEX IF EXISTS idx_event_store_event_data;
            DROP INDEX IF EXISTS idx_event_store_correlation_id;
            DROP INDEX IF EXISTS idx_event_store_occurred_at;
            DROP INDEX IF EXISTS idx_event_store_event_type;
            DROP INDEX IF EXISTS idx_event_store_aggregate_id;
            DROP TABLE IF EXISTS event_store;
        """)
