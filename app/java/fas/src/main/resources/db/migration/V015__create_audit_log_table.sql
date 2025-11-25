-- ==========================================
-- 監査ログテーブルの作成
-- ==========================================

-- 監査ログテーブル（Append-Onlyで不変）
CREATE TABLE IF NOT EXISTS "監査ログ" (
    "ログID" BIGSERIAL PRIMARY KEY,
    "エンティティ種別" VARCHAR(50) NOT NULL,
    "エンティティID" VARCHAR(100) NOT NULL,
    "操作種別" VARCHAR(20) NOT NULL,
    "ユーザーID" VARCHAR(100) NOT NULL,
    "ユーザー名" VARCHAR(200) NOT NULL,
    "タイムスタンプ" TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "変更前の値" JSONB,
    "変更後の値" JSONB,
    "変更内容" JSONB,
    "理由" TEXT,
    "IPアドレス" VARCHAR(45),
    "ユーザーエージェント" TEXT,
    "作成日時" TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);

-- インデックス作成
CREATE INDEX "idx_監査ログ_エンティティ" ON "監査ログ"("エンティティ種別", "エンティティID");
CREATE INDEX "idx_監査ログ_ユーザー" ON "監査ログ"("ユーザーID");
CREATE INDEX "idx_監査ログ_タイムスタンプ" ON "監査ログ"("タイムスタンプ");
CREATE INDEX "idx_監査ログ_操作種別" ON "監査ログ"("操作種別");

-- JSONB 列にもインデックス（特定のキーを検索する場合）
CREATE INDEX "idx_監査ログ_変更内容" ON "監査ログ" USING GIN ("変更内容");

-- CHECK制約
ALTER TABLE "監査ログ"
  ADD CONSTRAINT "check_操作種別"
  CHECK ("操作種別" IN ('CREATE', 'UPDATE', 'DELETE'));

-- コメント追加
COMMENT ON TABLE "監査ログ" IS '監査ログテーブル（Append-Onlyで不変、削除・更新禁止）';
COMMENT ON COLUMN "監査ログ"."エンティティ種別" IS 'エンティティ種別（Journal, Account等）';
COMMENT ON COLUMN "監査ログ"."エンティティID" IS 'エンティティの一意識別子';
COMMENT ON COLUMN "監査ログ"."操作種別" IS '操作種別（CREATE, UPDATE, DELETE）';
COMMENT ON COLUMN "監査ログ"."ユーザーID" IS '操作を実行したユーザーのID';
COMMENT ON COLUMN "監査ログ"."ユーザー名" IS '操作を実行したユーザーの名前';
COMMENT ON COLUMN "監査ログ"."タイムスタンプ" IS '操作が実行された日時';
COMMENT ON COLUMN "監査ログ"."変更前の値" IS '変更前の値（UPDATE, DELETE時）';
COMMENT ON COLUMN "監査ログ"."変更後の値" IS '変更後の値（UPDATE時）';
COMMENT ON COLUMN "監査ログ"."変更内容" IS '変更内容（CREATE時）';
COMMENT ON COLUMN "監査ログ"."理由" IS '操作理由（任意）';
COMMENT ON COLUMN "監査ログ"."IPアドレス" IS '操作元のIPアドレス';
COMMENT ON COLUMN "監査ログ"."ユーザーエージェント" IS 'ユーザーエージェント情報';
