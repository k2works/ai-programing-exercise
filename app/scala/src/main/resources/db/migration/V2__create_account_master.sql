-- 勘定科目種別のenum型を作成
DO $$ BEGIN
    CREATE TYPE account_type AS ENUM ('資産', '負債', '純資産', '収益', '費用');
EXCEPTION
    WHEN duplicate_object THEN null;
END $$;

-- 勘定科目マスタテーブルを作成
CREATE TABLE IF NOT EXISTS "勘定科目マスタ" (
    "勘定科目ID" SERIAL PRIMARY KEY,
    "勘定科目コード" VARCHAR(20) UNIQUE NOT NULL,
    "勘定科目名" VARCHAR(100) NOT NULL,
    "勘定科目種別" account_type NOT NULL,
    "残高" DECIMAL(15,2) DEFAULT 0 NOT NULL,
    "作成日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "更新日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL
);

-- テーブルコメント
COMMENT ON TABLE "勘定科目マスタ" IS
    '勘定科目マスタ（財務会計システムの基本となる勘定科目情報）';

-- カラムコメント
COMMENT ON COLUMN "勘定科目マスタ"."勘定科目ID" IS '勘定科目ID（主キー）';
COMMENT ON COLUMN "勘定科目マスタ"."勘定科目コード" IS '勘定科目コード（例：1000, 2000）';
COMMENT ON COLUMN "勘定科目マスタ"."勘定科目名" IS '勘定科目名（例：現金、売掛金）';
COMMENT ON COLUMN "勘定科目マスタ"."勘定科目種別" IS '勘定科目種別（資産、負債、純資産、収益、費用）';
COMMENT ON COLUMN "勘定科目マスタ"."残高" IS '残高';
COMMENT ON COLUMN "勘定科目マスタ"."作成日時" IS '作成日時';
COMMENT ON COLUMN "勘定科目マスタ"."更新日時" IS '更新日時';
