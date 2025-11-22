-- イベントストアテーブル
-- イベントソーシングパターンの中核となるテーブル
-- すべてのドメインイベントを Append-Only で保存

CREATE TABLE IF NOT EXISTS "イベントストア" (
    -- 主キー（グローバルシーケンス番号）
    "イベントID" BIGSERIAL PRIMARY KEY,

    -- Aggregate 識別子
    "集約ID" VARCHAR(100) NOT NULL,
    "集約種別" VARCHAR(50) NOT NULL,

    -- イベントメタデータ
    "イベント種別" VARCHAR(100) NOT NULL,
    "イベントバージョン" INTEGER NOT NULL DEFAULT 1,

    -- イベントデータ（JSONB形式）
    "イベントデータ" JSONB NOT NULL,

    -- メタデータ
    "発生日時" TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "ユーザーID" VARCHAR(100),
    "相関ID" VARCHAR(100),
    "因果ID" VARCHAR(100),

    -- 楽観的ロック（同時実行制御）
    "シーケンス番号" INTEGER NOT NULL,

    -- 一意性制約（同一 Aggregate の sequence_number は一意）
    CONSTRAINT "uk_event_store_aggregate_sequence"
        UNIQUE ("集約ID", "シーケンス番号")
);

-- インデックス設計

-- Aggregate 検索用インデックス（最も頻繁に使用される）
CREATE INDEX "idx_event_store_aggregate_id"
    ON "イベントストア"("集約ID", "シーケンス番号");

-- イベントタイプ検索用インデックス
CREATE INDEX "idx_event_store_event_type"
    ON "イベントストア"("イベント種別");

-- 時系列検索用インデックス
CREATE INDEX "idx_event_store_occurred_at"
    ON "イベントストア"("発生日時");

-- 相関ID検索用インデックス（関連イベントのトレース）
CREATE INDEX "idx_event_store_correlation_id"
    ON "イベントストア"("相関ID");

-- JSONB用のGINインデックス（イベントデータ内の検索を高速化）
CREATE INDEX "idx_event_store_event_data"
    ON "イベントストア" USING GIN ("イベントデータ");

-- 集約種別とイベント種別の複合インデックス
CREATE INDEX "idx_event_store_aggregate_event_type"
    ON "イベントストア"("集約種別", "イベント種別");

-- テーブルコメント
COMMENT ON TABLE "イベントストア" IS 'イベントソーシングパターンのイベントストア - すべてのドメインイベントを Append-Only で保存';

-- カラムコメント
COMMENT ON COLUMN "イベントストア"."イベントID" IS 'グローバルな順序を保証するシーケンス番号';
COMMENT ON COLUMN "イベントストア"."集約ID" IS 'Aggregate のユニーク識別子（例: journal-entry-2024001）';
COMMENT ON COLUMN "イベントストア"."集約種別" IS 'Aggregate の型（例: JournalEntry, Account）';
COMMENT ON COLUMN "イベントストア"."イベント種別" IS 'イベントの型（例: JournalEntryCreatedEvent）';
COMMENT ON COLUMN "イベントストア"."イベントバージョン" IS 'イベント定義のバージョン（スキーマ進化に対応）';
COMMENT ON COLUMN "イベントストア"."イベントデータ" IS 'イベントのペイロード（JSONB形式）';
COMMENT ON COLUMN "イベントストア"."発生日時" IS 'イベントが発生した日時';
COMMENT ON COLUMN "イベントストア"."ユーザーID" IS 'イベントを発生させたユーザーID';
COMMENT ON COLUMN "イベントストア"."相関ID" IS '関連する一連のイベントをグループ化するID';
COMMENT ON COLUMN "イベントストア"."因果ID" IS '因果関係のあるイベントID（このイベントによって発生）';
COMMENT ON COLUMN "イベントストア"."シーケンス番号" IS 'Aggregate 内でのイベントの順序番号（楽観的ロック）';
