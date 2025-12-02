-- イベントストアテーブル（イベントソーシング用）
CREATE TABLE IF NOT EXISTS "イベントストア" (
    "ID" BIGSERIAL PRIMARY KEY,
    "集約ID" VARCHAR(255) NOT NULL,
    "集約種別" VARCHAR(100) NOT NULL,
    "シーケンス番号" BIGINT NOT NULL,
    "イベント種別" VARCHAR(100) NOT NULL,
    "イベントデータ" JSONB NOT NULL,
    "メタデータ" JSONB,
    "発生日時" TIMESTAMP WITH TIME ZONE NOT NULL,
    "作成日時" TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,

    -- 楽観的ロック用の一意制約
    CONSTRAINT uk_aggregate_sequence UNIQUE ("集約ID", "シーケンス番号")
);

-- インデックス
CREATE INDEX idx_event_store_aggregate_id ON "イベントストア"("集約ID");
CREATE INDEX idx_event_store_aggregate_type ON "イベントストア"("集約種別");
CREATE INDEX idx_event_store_occurred_at ON "イベントストア"("発生日時");
CREATE INDEX idx_event_store_event_type ON "イベントストア"("イベント種別");

-- JSONB のインデックス（GIN）
CREATE INDEX idx_event_store_event_data ON "イベントストア" USING GIN ("イベントデータ");

-- コメント
COMMENT ON TABLE "イベントストア" IS 'イベントストア（Append-Only）';
COMMENT ON COLUMN "イベントストア"."集約ID" IS 'Aggregate の ID';
COMMENT ON COLUMN "イベントストア"."集約種別" IS 'Aggregate の種別（JournalEntry など）';
COMMENT ON COLUMN "イベントストア"."シーケンス番号" IS 'イベントのシーケンス番号（楽観的ロック）';
COMMENT ON COLUMN "イベントストア"."イベント種別" IS 'イベントの種別（Created, Approved など）';
COMMENT ON COLUMN "イベントストア"."イベントデータ" IS 'イベントデータ（JSONB）';
COMMENT ON COLUMN "イベントストア"."メタデータ" IS 'メタデータ（ユーザーID、IPアドレスなど）';
COMMENT ON COLUMN "イベントストア"."発生日時" IS 'イベント発生日時';

-- スナップショットテーブル
CREATE TABLE IF NOT EXISTS "集約スナップショット" (
    "集約ID" VARCHAR(255) PRIMARY KEY,
    "集約種別" VARCHAR(100) NOT NULL,
    "バージョン" BIGINT NOT NULL,
    "スナップショットデータ" JSONB NOT NULL,
    "作成日時" TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP
);

-- インデックス
CREATE INDEX idx_aggregate_snapshot_type ON "集約スナップショット"("集約種別");
CREATE INDEX idx_aggregate_snapshot_version ON "集約スナップショット"("バージョン");

-- コメント
COMMENT ON TABLE "集約スナップショット" IS 'Aggregate のスナップショット';
COMMENT ON COLUMN "集約スナップショット"."集約ID" IS 'Aggregate の ID';
COMMENT ON COLUMN "集約スナップショット"."集約種別" IS 'Aggregate の種別';
COMMENT ON COLUMN "集約スナップショット"."バージョン" IS 'スナップショット時点のバージョン';
COMMENT ON COLUMN "集約スナップショット"."スナップショットデータ" IS 'スナップショットデータ（JSONB）';

-- 仕訳 Read Model テーブル（CQRS 用）
CREATE TABLE IF NOT EXISTS "仕訳リードモデル" (
    "集約ID" VARCHAR(255) PRIMARY KEY,
    "仕訳日付" DATE NOT NULL,
    "摘要" TEXT NOT NULL,
    "借方合計" DECIMAL(15, 2) NOT NULL DEFAULT 0,
    "貸方合計" DECIMAL(15, 2) NOT NULL DEFAULT 0,
    "ステータス" VARCHAR(20) NOT NULL DEFAULT 'DRAFT',
    "作成日時" TIMESTAMP WITH TIME ZONE NOT NULL,
    "承認日時" TIMESTAMP WITH TIME ZONE,
    "承認者" VARCHAR(100),
    "バージョン" BIGINT NOT NULL DEFAULT 0
);

-- インデックス
CREATE INDEX idx_journal_read_model_date ON "仕訳リードモデル"("仕訳日付");
CREATE INDEX idx_journal_read_model_status ON "仕訳リードモデル"("ステータス");

-- コメント
COMMENT ON TABLE "仕訳リードモデル" IS '仕訳の Read Model（CQRS 用）';
COMMENT ON COLUMN "仕訳リードモデル"."ステータス" IS 'DRAFT, APPROVED, DELETED';
