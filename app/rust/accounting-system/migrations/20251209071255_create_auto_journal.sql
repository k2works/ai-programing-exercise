-- 1. 自動仕訳管理テーブル
CREATE TABLE "自動仕訳管理" (
    "id" BIGSERIAL PRIMARY KEY,
    "source_table_name" VARCHAR(100) NOT NULL UNIQUE,
    "last_processed_at" TIMESTAMP NOT NULL,
    "created_at" TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updated_at" TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP
);

-- インデックス作成
CREATE INDEX "idx_自動仕訳管理_source_table" ON "自動仕訳管理"("source_table_name");

-- 2. 自動仕訳パターンテーブル
CREATE TABLE "自動仕訳パターン" (
    "id" BIGSERIAL PRIMARY KEY,
    "pattern_code" VARCHAR(20) NOT NULL UNIQUE,
    "pattern_name" VARCHAR(100) NOT NULL,
    "source_table_name" VARCHAR(100) NOT NULL,
    "description" VARCHAR(500),
    "is_active" BOOLEAN NOT NULL DEFAULT true,
    "created_at" TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updated_at" TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP
);

-- インデックス作成
CREATE INDEX "idx_自動仕訳パターン_pattern_code" ON "自動仕訳パターン"("pattern_code");
CREATE INDEX "idx_自動仕訳パターン_is_active" ON "自動仕訳パターン"("is_active");

-- 3. 自動仕訳パターン明細テーブル
CREATE TABLE "自動仕訳パターン明細" (
    "id" BIGSERIAL PRIMARY KEY,
    "pattern_id" BIGINT NOT NULL,
    "line_number" INTEGER NOT NULL,
    "debit_credit_flag" CHAR(1) NOT NULL,
    "account_code" VARCHAR(10) NOT NULL,
    "amount_expression" VARCHAR(200) NOT NULL,
    "description_template" VARCHAR(200),
    "created_at" TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updated_at" TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT "uk_自動仕訳パターン明細_pattern_line" UNIQUE ("pattern_id", "line_number", "debit_credit_flag")
);

-- インデックス作成
CREATE INDEX "idx_自動仕訳パターン明細_pattern_id" ON "自動仕訳パターン明細"("pattern_id");

-- 外部キー制約
ALTER TABLE "自動仕訳パターン明細"
    ADD CONSTRAINT "fk_自動仕訳パターン明細_パターン"
    FOREIGN KEY ("pattern_id")
    REFERENCES "自動仕訳パターン"("id")
    ON DELETE CASCADE;

-- CHECK制約
ALTER TABLE "自動仕訳パターン明細"
    ADD CONSTRAINT "check_貸借区分_auto"
    CHECK ("debit_credit_flag" IN ('D', 'C'));

-- 4. 自動仕訳実行ログテーブル
CREATE TABLE "自動仕訳実行ログ" (
    "id" BIGSERIAL PRIMARY KEY,
    "pattern_id" BIGINT NOT NULL,
    "executed_at" TIMESTAMP NOT NULL,
    "processed_count" INTEGER NOT NULL DEFAULT 0,
    "generated_count" INTEGER NOT NULL DEFAULT 0,
    "status" VARCHAR(20) NOT NULL,
    "message" VARCHAR(500),
    "error_detail" TEXT,
    "created_at" TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP
);

-- インデックス作成
CREATE INDEX "idx_自動仕訳実行ログ_pattern_id" ON "自動仕訳実行ログ"("pattern_id");
CREATE INDEX "idx_自動仕訳実行ログ_executed_at" ON "自動仕訳実行ログ"("executed_at");
CREATE INDEX "idx_自動仕訳実行ログ_status" ON "自動仕訳実行ログ"("status");

-- 外部キー制約
ALTER TABLE "自動仕訳実行ログ"
    ADD CONSTRAINT "fk_自動仕訳実行ログ_パターン"
    FOREIGN KEY ("pattern_id")
    REFERENCES "自動仕訳パターン"("id")
    ON DELETE CASCADE;

-- CHECK制約
ALTER TABLE "自動仕訳実行ログ"
    ADD CONSTRAINT "check_status"
    CHECK ("status" IN ('success', 'error', 'running'));

ALTER TABLE "自動仕訳実行ログ"
    ADD CONSTRAINT "check_processed_count"
    CHECK ("processed_count" >= 0);

ALTER TABLE "自動仕訳実行ログ"
    ADD CONSTRAINT "check_generated_count"
    CHECK ("generated_count" >= 0);

-- テーブルコメント
COMMENT ON TABLE "自動仕訳管理" IS '自動仕訳の最終処理日時を管理するテーブル（日付管理方式）';
COMMENT ON TABLE "自動仕訳パターン" IS '自動仕訳の生成パターンを定義するテーブル';
COMMENT ON TABLE "自動仕訳パターン明細" IS '自動仕訳パターンの明細（勘定科目と金額式）';
COMMENT ON TABLE "自動仕訳実行ログ" IS '自動仕訳の実行履歴と監査証跡';

-- カラムコメント
COMMENT ON COLUMN "自動仕訳管理"."source_table_name" IS '連携元テーブル名（一意）';
COMMENT ON COLUMN "自動仕訳管理"."last_processed_at" IS '最終処理日時';
COMMENT ON COLUMN "自動仕訳パターン"."pattern_code" IS 'パターン識別コード';
COMMENT ON COLUMN "自動仕訳パターン"."is_active" IS '有効/無効フラグ';
COMMENT ON COLUMN "自動仕訳パターン明細"."amount_expression" IS '金額計算式（例: total_amount, price * quantity）';
COMMENT ON COLUMN "自動仕訳実行ログ"."status" IS '実行ステータス（success, error, running）';
COMMENT ON COLUMN "自動仕訳実行ログ"."processed_count" IS '処理対象データ件数';
COMMENT ON COLUMN "自動仕訳実行ログ"."generated_count" IS '生成仕訳件数';
