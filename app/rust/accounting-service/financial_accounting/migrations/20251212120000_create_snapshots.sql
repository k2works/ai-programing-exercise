-- スナップショットテーブル
CREATE TABLE IF NOT EXISTS snapshots (
    snapshot_id BIGSERIAL PRIMARY KEY,
    aggregate_type VARCHAR(100) NOT NULL,
    aggregate_id VARCHAR(255) NOT NULL,
    version BIGINT NOT NULL,
    snapshot_data JSONB NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW(),

    -- 集約ごとに最新のスナップショットのみ保持
    UNIQUE (aggregate_type, aggregate_id)
);

-- インデックス
CREATE INDEX idx_snapshots_aggregate
ON snapshots(aggregate_type, aggregate_id);

CREATE INDEX idx_snapshots_version
ON snapshots(aggregate_type, aggregate_id, version DESC);

-- コメント
COMMENT ON TABLE snapshots IS 'イベントソーシング用スナップショット';
COMMENT ON COLUMN snapshots.aggregate_type IS '集約のタイプ（例：Journal, Account）';
COMMENT ON COLUMN snapshots.aggregate_id IS '集約の一意識別子';
COMMENT ON COLUMN snapshots.version IS 'スナップショット作成時点のイベントバージョン（シーケンス番号）';
COMMENT ON COLUMN snapshots.snapshot_data IS '集約の状態（JSON形式）';
