-- イベントストアテーブル
CREATE TABLE IF NOT EXISTS event_store (
    -- シーケンス番号（主キー、自動採番）
    sequence_number BIGSERIAL PRIMARY KEY,

    -- 集約の情報
    aggregate_type VARCHAR(100) NOT NULL,
    aggregate_id VARCHAR(255) NOT NULL,

    -- イベントの情報
    event_type VARCHAR(100) NOT NULL,
    event_version INT NOT NULL DEFAULT 1,
    event_data JSONB NOT NULL,

    -- メタデータ
    metadata JSONB,
    occurred_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW(),
    user_id VARCHAR(100),
    correlation_id UUID,
    causation_id UUID,

    -- インデックス用の複合制約
    CONSTRAINT unique_aggregate_sequence UNIQUE (aggregate_type, aggregate_id, sequence_number)
);

-- 集約IDでのイベント検索用インデックス
CREATE INDEX idx_event_store_aggregate
ON event_store(aggregate_type, aggregate_id, sequence_number);

-- イベントタイプでの検索用インデックス
CREATE INDEX idx_event_store_event_type
ON event_store(event_type);

-- 発生日時での検索用インデックス
CREATE INDEX idx_event_store_occurred_at
ON event_store(occurred_at);

-- Correlation ID でのイベント追跡用インデックス
CREATE INDEX idx_event_store_correlation
ON event_store(correlation_id) WHERE correlation_id IS NOT NULL;

-- JSONB データの GIN インデックス（イベントデータ内の検索用）
CREATE INDEX idx_event_store_data
ON event_store USING GIN(event_data);

-- コメント
COMMENT ON TABLE event_store IS 'イベントソーシング用の追記専用イベントストア';
COMMENT ON COLUMN event_store.sequence_number IS 'グローバルなイベントシーケンス番号';
COMMENT ON COLUMN event_store.aggregate_type IS '集約のタイプ（例：Journal, Account）';
COMMENT ON COLUMN event_store.aggregate_id IS '集約の一意識別子';
COMMENT ON COLUMN event_store.event_type IS 'イベントのタイプ名';
COMMENT ON COLUMN event_store.event_version IS 'イベントスキーマのバージョン';
COMMENT ON COLUMN event_store.event_data IS 'イベントデータ（JSON形式）';
COMMENT ON COLUMN event_store.correlation_id IS '関連するイベント群を追跡するID';
COMMENT ON COLUMN event_store.causation_id IS 'このイベントの原因となったイベントのID';
