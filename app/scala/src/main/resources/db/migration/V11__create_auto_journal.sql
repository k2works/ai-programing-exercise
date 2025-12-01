-- ==========================================
-- 自動仕訳テーブルの作成
-- ==========================================

-- 1. 自動仕訳管理テーブル
CREATE TABLE IF NOT EXISTS "自動仕訳管理" (
    "自動仕訳管理ID" VARCHAR(20) PRIMARY KEY,
    "自動仕訳名" VARCHAR(100) NOT NULL,
    "ソーステーブル名" VARCHAR(100) NOT NULL,
    "最終処理日時" TIMESTAMP,
    "有効フラグ" INTEGER DEFAULT 1 NOT NULL,
    "作成日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "更新日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL
);

-- CHECK制約
ALTER TABLE "自動仕訳管理"
  ADD CONSTRAINT "check_有効フラグ"
  CHECK ("有効フラグ" IN (0, 1));

-- コメント
COMMENT ON TABLE "自動仕訳管理" IS '自動仕訳管理マスタ（日付管理方式）';
COMMENT ON COLUMN "自動仕訳管理"."最終処理日時" IS '差分処理のための最終処理日時';
COMMENT ON COLUMN "自動仕訳管理"."有効フラグ" IS '0=無効、1=有効';

-- 2. 自動仕訳パターンテーブル
CREATE TABLE IF NOT EXISTS "自動仕訳パターン" (
    "パターンID" VARCHAR(20) PRIMARY KEY,
    "自動仕訳管理ID" VARCHAR(20) NOT NULL,
    "パターン名" VARCHAR(100) NOT NULL,
    "条件式" TEXT,
    "優先順位" INTEGER DEFAULT 0 NOT NULL,
    "有効フラグ" INTEGER DEFAULT 1 NOT NULL,
    "作成日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "更新日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    FOREIGN KEY ("自動仕訳管理ID") REFERENCES "自動仕訳管理" ("自動仕訳管理ID") ON DELETE CASCADE
);

-- インデックス
CREATE INDEX "idx_自動仕訳パターン_管理ID" ON "自動仕訳パターン"("自動仕訳管理ID");

-- CHECK制約
ALTER TABLE "自動仕訳パターン"
  ADD CONSTRAINT "check_パターン有効フラグ"
  CHECK ("有効フラグ" IN (0, 1));

-- コメント
COMMENT ON TABLE "自動仕訳パターン" IS '自動仕訳パターン（条件に基づく仕訳生成ルール）';
COMMENT ON COLUMN "自動仕訳パターン"."条件式" IS 'SQL WHERE句の条件（NULL=常にマッチ）';
COMMENT ON COLUMN "自動仕訳パターン"."優先順位" IS '数値が小さいほど優先';

-- 3. 自動仕訳パターン明細テーブル
CREATE TABLE IF NOT EXISTS "自動仕訳パターン明細" (
    "パターンID" VARCHAR(20),
    "行番号" INTEGER,
    "貸借区分" CHAR(1) NOT NULL,
    "勘定科目コード" VARCHAR(20) NOT NULL,
    "金額式" TEXT NOT NULL,
    "摘要テンプレート" VARCHAR(100),
    "作成日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "更新日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    PRIMARY KEY ("パターンID", "行番号"),
    FOREIGN KEY ("パターンID") REFERENCES "自動仕訳パターン" ("パターンID") ON DELETE CASCADE,
    FOREIGN KEY ("勘定科目コード") REFERENCES "勘定科目マスタ" ("勘定科目コード")
);

-- インデックス
CREATE INDEX "idx_自動仕訳パターン明細_パターンID" ON "自動仕訳パターン明細"("パターンID");

-- CHECK制約
ALTER TABLE "自動仕訳パターン明細"
  ADD CONSTRAINT "check_明細貸借区分"
  CHECK ("貸借区分" IN ('D', 'C'));

-- コメント
COMMENT ON TABLE "自動仕訳パターン明細" IS '自動仕訳パターン明細（仕訳生成の詳細ルール）';
COMMENT ON COLUMN "自動仕訳パターン明細"."貸借区分" IS 'D=借方、C=貸方';
COMMENT ON COLUMN "自動仕訳パターン明細"."金額式" IS 'SQL式（例：amount、amount * 0.1）';
COMMENT ON COLUMN "自動仕訳パターン明細"."摘要テンプレート" IS '変数置換可能なテンプレート';

-- 4. 自動仕訳実行ログテーブル
CREATE TABLE IF NOT EXISTS "自動仕訳実行ログ" (
    "実行ログID" BIGSERIAL PRIMARY KEY,
    "自動仕訳管理ID" VARCHAR(20) NOT NULL,
    "実行開始日時" TIMESTAMP NOT NULL,
    "実行終了日時" TIMESTAMP,
    "処理件数" INTEGER DEFAULT 0 NOT NULL,
    "成功件数" INTEGER DEFAULT 0 NOT NULL,
    "エラー件数" INTEGER DEFAULT 0 NOT NULL,
    "実行結果" VARCHAR(20) NOT NULL,
    "エラーメッセージ" TEXT,
    "作成日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    FOREIGN KEY ("自動仕訳管理ID") REFERENCES "自動仕訳管理" ("自動仕訳管理ID")
);

-- インデックス
CREATE INDEX "idx_自動仕訳実行ログ_管理ID" ON "自動仕訳実行ログ"("自動仕訳管理ID");
CREATE INDEX "idx_自動仕訳実行ログ_実行日時" ON "自動仕訳実行ログ"("実行開始日時");

-- CHECK制約
ALTER TABLE "自動仕訳実行ログ"
  ADD CONSTRAINT "check_実行結果"
  CHECK ("実行結果" IN ('RUNNING', 'SUCCESS', 'FAILED', 'PARTIAL'));

-- コメント
COMMENT ON TABLE "自動仕訳実行ログ" IS '自動仕訳実行ログ（監査証跡）';
COMMENT ON COLUMN "自動仕訳実行ログ"."実行結果" IS 'RUNNING=実行中、SUCCESS=成功、FAILED=失敗、PARTIAL=部分成功';
