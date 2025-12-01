-- 勘定科目構成マスタ
CREATE TABLE IF NOT EXISTS "勘定科目構成マスタ" (
    "勘定科目コード" VARCHAR(20) PRIMARY KEY,
    "チルダパス" VARCHAR(200) NOT NULL,
    "階層レベル" INTEGER NOT NULL,
    "親科目コード" VARCHAR(20),
    "作成日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "更新日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,

    -- 外部キー制約
    CONSTRAINT fk_account_structure_code
        FOREIGN KEY ("勘定科目コード")
        REFERENCES "勘定科目マスタ" ("勘定科目コード")
        ON DELETE CASCADE,

    -- 親科目コードも勘定科目マスタを参照
    CONSTRAINT fk_account_structure_parent
        FOREIGN KEY ("親科目コード")
        REFERENCES "勘定科目マスタ" ("勘定科目コード")
        ON DELETE SET NULL,

    -- CHECK制約
    CONSTRAINT chk_hierarchy_level
        CHECK ("階層レベル" > 0 AND "階層レベル" <= 10)
);

-- テーブルコメント
COMMENT ON TABLE "勘定科目構成マスタ" IS
    '勘定科目構成マスタ（勘定科目の階層構造を管理）';

-- カラムコメント
COMMENT ON COLUMN "勘定科目構成マスタ"."勘定科目コード" IS '勘定科目コード（主キー、外部キー）';
COMMENT ON COLUMN "勘定科目構成マスタ"."チルダパス" IS 'チルダパス（例：11000~11100~11190~11110）';
COMMENT ON COLUMN "勘定科目構成マスタ"."階層レベル" IS '階層レベル（ルート=1）';
COMMENT ON COLUMN "勘定科目構成マスタ"."親科目コード" IS '親科目コード';

-- インデックス
CREATE INDEX IF NOT EXISTS idx_account_structure_path
    ON "勘定科目構成マスタ" ("チルダパス");

CREATE INDEX IF NOT EXISTS idx_account_structure_level
    ON "勘定科目構成マスタ" ("階層レベル");

CREATE INDEX IF NOT EXISTS idx_account_structure_parent
    ON "勘定科目構成マスタ" ("親科目コード")
    WHERE "親科目コード" IS NOT NULL;
