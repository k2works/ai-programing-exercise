-- Aggregate スナップショットテーブル（イベントソーシング最適化）
CREATE TABLE IF NOT EXISTS aggregate_snapshot (
    aggregate_id VARCHAR(100) NOT NULL,
    aggregate_type VARCHAR(50) NOT NULL,
    version INTEGER NOT NULL,
    snapshot_data JSONB NOT NULL,
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY (aggregate_id, version)
);

-- 最新のスナップショットを高速取得するためのインデックス
CREATE INDEX idx_aggregate_snapshot_latest
    ON aggregate_snapshot(aggregate_id, version DESC);

COMMENT ON TABLE aggregate_snapshot IS 'Aggregate スナップショット（イベント再生の最適化）';
COMMENT ON COLUMN aggregate_snapshot.aggregate_id IS '集約ID';
COMMENT ON COLUMN aggregate_snapshot.aggregate_type IS '集約種別';
COMMENT ON COLUMN aggregate_snapshot.version IS 'バージョン（スナップショット時点のイベント数）';
COMMENT ON COLUMN aggregate_snapshot.snapshot_data IS 'スナップショットデータ（JSON）';
COMMENT ON COLUMN aggregate_snapshot.created_at IS '作成日時';
