-- ==========================================
-- 自動仕訳テーブルの作成
-- ==========================================

-- 1. 自動仕訳管理テーブル
CREATE TABLE IF NOT EXISTS "自動仕訳管理" (
    "ID" BIGSERIAL PRIMARY KEY,
    "ソーステーブル名" VARCHAR(100) UNIQUE NOT NULL,
    "最終処理日時" TIMESTAMP,
    "作成日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "更新日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL
);

-- 2. 自動仕訳パターンテーブル
CREATE TABLE IF NOT EXISTS "自動仕訳パターン" (
    "ID" BIGSERIAL PRIMARY KEY,
    "パターンコード" VARCHAR(20) UNIQUE NOT NULL,
    "パターン名" VARCHAR(100) NOT NULL,
    "ソーステーブル名" VARCHAR(100) NOT NULL,
    "説明" VARCHAR(500),
    "有効フラグ" BOOLEAN DEFAULT true NOT NULL,
    "作成日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "更新日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL
);

-- 3. 自動仕訳パターン明細テーブル
CREATE TABLE IF NOT EXISTS "自動仕訳パターン明細" (
    "ID" BIGSERIAL PRIMARY KEY,
    "パターンID" BIGINT NOT NULL,
    "行番号" INTEGER NOT NULL,
    "貸借区分" CHAR(1) NOT NULL,
    "勘定科目コード" VARCHAR(10) NOT NULL,
    "金額式" VARCHAR(200) NOT NULL,
    "摘要テンプレート" VARCHAR(200),
    "作成日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "更新日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    FOREIGN KEY ("パターンID") REFERENCES "自動仕訳パターン" ("ID") ON DELETE CASCADE,
    FOREIGN KEY ("勘定科目コード") REFERENCES "勘定科目マスタ" ("勘定科目コード"),
    UNIQUE ("パターンID", "行番号")
);

-- 4. 自動仕訳実行ログテーブル
CREATE TABLE IF NOT EXISTS "自動仕訳実行ログ" (
    "ID" BIGSERIAL PRIMARY KEY,
    "パターンID" BIGINT NOT NULL,
    "実行日時" TIMESTAMP NOT NULL,
    "処理件数" INTEGER DEFAULT 0 NOT NULL,
    "生成件数" INTEGER DEFAULT 0 NOT NULL,
    "ステータス" VARCHAR(20) NOT NULL,
    "メッセージ" VARCHAR(500),
    "エラー詳細" TEXT,
    "作成日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    FOREIGN KEY ("パターンID") REFERENCES "自動仕訳パターン" ("ID")
);

-- インデックス作成
CREATE INDEX "idx_自動仕訳管理_ソーステーブル名" ON "自動仕訳管理"("ソーステーブル名");
CREATE INDEX "idx_自動仕訳パターン_パターンコード" ON "自動仕訳パターン"("パターンコード");
CREATE INDEX "idx_自動仕訳パターン_有効フラグ" ON "自動仕訳パターン"("有効フラグ");
CREATE INDEX "idx_自動仕訳パターン明細_パターンID" ON "自動仕訳パターン明細"("パターンID");
CREATE INDEX "idx_自動仕訳実行ログ_パターンID" ON "自動仕訳実行ログ"("パターンID");
CREATE INDEX "idx_自動仕訳実行ログ_実行日時" ON "自動仕訳実行ログ"("実行日時");
CREATE INDEX "idx_自動仕訳実行ログ_ステータス" ON "自動仕訳実行ログ"("ステータス");

-- CHECK 制約
ALTER TABLE "自動仕訳パターン明細"
  ADD CONSTRAINT "check_貸借区分"
  CHECK ("貸借区分" IN ('D', 'C'));

ALTER TABLE "自動仕訳実行ログ"
  ADD CONSTRAINT "check_ステータス"
  CHECK ("ステータス" IN ('SUCCESS', 'FAILURE', 'RUNNING'));

ALTER TABLE "自動仕訳実行ログ"
  ADD CONSTRAINT "check_処理件数"
  CHECK ("処理件数" >= 0);

ALTER TABLE "自動仕訳実行ログ"
  ADD CONSTRAINT "check_生成件数"
  CHECK ("生成件数" >= 0);

-- テーブルコメント
COMMENT ON TABLE "自動仕訳管理" IS '自動仕訳管理（日付管理方式による差分処理管理）';
COMMENT ON TABLE "自動仕訳パターン" IS '自動仕訳パターン（仕訳生成ルールの定義）';
COMMENT ON TABLE "自動仕訳パターン明細" IS '自動仕訳パターン明細（パターンの詳細定義）';
COMMENT ON TABLE "自動仕訳実行ログ" IS '自動仕訳実行ログ（実行履歴と監査証跡）';

-- カラムコメント（自動仕訳管理）
COMMENT ON COLUMN "自動仕訳管理"."ID" IS 'ID（主キー）';
COMMENT ON COLUMN "自動仕訳管理"."ソーステーブル名" IS 'ソーステーブル名（一意制約）';
COMMENT ON COLUMN "自動仕訳管理"."最終処理日時" IS '最終処理日時（差分処理用）';
COMMENT ON COLUMN "自動仕訳管理"."作成日時" IS '作成日時';
COMMENT ON COLUMN "自動仕訳管理"."更新日時" IS '更新日時';

-- カラムコメント（自動仕訳パターン）
COMMENT ON COLUMN "自動仕訳パターン"."ID" IS 'ID（主キー）';
COMMENT ON COLUMN "自動仕訳パターン"."パターンコード" IS 'パターンコード（一意制約）';
COMMENT ON COLUMN "自動仕訳パターン"."パターン名" IS 'パターン名';
COMMENT ON COLUMN "自動仕訳パターン"."ソーステーブル名" IS 'ソーステーブル名';
COMMENT ON COLUMN "自動仕訳パターン"."説明" IS '説明';
COMMENT ON COLUMN "自動仕訳パターン"."有効フラグ" IS '有効フラグ（true=有効、false=無効）';
COMMENT ON COLUMN "自動仕訳パターン"."作成日時" IS '作成日時';
COMMENT ON COLUMN "自動仕訳パターン"."更新日時" IS '更新日時';

-- カラムコメント（自動仕訳パターン明細）
COMMENT ON COLUMN "自動仕訳パターン明細"."ID" IS 'ID（主キー）';
COMMENT ON COLUMN "自動仕訳パターン明細"."パターンID" IS 'パターンID（外部キー）';
COMMENT ON COLUMN "自動仕訳パターン明細"."行番号" IS '行番号';
COMMENT ON COLUMN "自動仕訳パターン明細"."貸借区分" IS '貸借区分（D=借方、C=貸方）';
COMMENT ON COLUMN "自動仕訳パターン明細"."勘定科目コード" IS '勘定科目コード';
COMMENT ON COLUMN "自動仕訳パターン明細"."金額式" IS '金額式（例：amount, amount * 1.1）';
COMMENT ON COLUMN "自動仕訳パターン明細"."摘要テンプレート" IS '摘要テンプレート';
COMMENT ON COLUMN "自動仕訳パターン明細"."作成日時" IS '作成日時';
COMMENT ON COLUMN "自動仕訳パターン明細"."更新日時" IS '更新日時';

-- カラムコメント（自動仕訳実行ログ）
COMMENT ON COLUMN "自動仕訳実行ログ"."ID" IS 'ID（主キー）';
COMMENT ON COLUMN "自動仕訳実行ログ"."パターンID" IS 'パターンID（外部キー）';
COMMENT ON COLUMN "自動仕訳実行ログ"."実行日時" IS '実行日時';
COMMENT ON COLUMN "自動仕訳実行ログ"."処理件数" IS '処理件数';
COMMENT ON COLUMN "自動仕訳実行ログ"."生成件数" IS '生成件数';
COMMENT ON COLUMN "自動仕訳実行ログ"."ステータス" IS 'ステータス（SUCCESS, FAILURE, RUNNING）';
COMMENT ON COLUMN "自動仕訳実行ログ"."メッセージ" IS 'メッセージ';
COMMENT ON COLUMN "自動仕訳実行ログ"."エラー詳細" IS 'エラー詳細';
COMMENT ON COLUMN "自動仕訳実行ログ"."作成日時" IS '作成日時';
