-- 勘定科目構成マスタ
CREATE TABLE "勘定科目構成マスタ" (
    "勘定科目コード" VARCHAR(20) PRIMARY KEY,
    "勘定科目パス" VARCHAR(200) NOT NULL,
    "階層レベル" INTEGER NOT NULL DEFAULT 1,
    "親科目コード" VARCHAR(20),
    "表示順序" INTEGER NOT NULL DEFAULT 0,
    "作成日時" TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "更新日時" TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP
);

-- 外部キー制約（勘定科目マスタへの参照）
ALTER TABLE "勘定科目構成マスタ"
    ADD CONSTRAINT fk_account_structure_account
    FOREIGN KEY ("勘定科目コード")
    REFERENCES "勘定科目マスタ" ("勘定科目コード")
    ON DELETE CASCADE;

-- テーブルとカラムのコメント
COMMENT ON TABLE "勘定科目構成マスタ" IS '勘定科目の階層構造を管理するマスタテーブル';
COMMENT ON COLUMN "勘定科目構成マスタ"."勘定科目コード" IS '勘定科目コード';
COMMENT ON COLUMN "勘定科目構成マスタ"."勘定科目パス" IS 'チルダ連結形式のパス（例: 11~11000~11190~11110）';
COMMENT ON COLUMN "勘定科目構成マスタ"."階層レベル" IS '階層の深さ（ルート=1）';
COMMENT ON COLUMN "勘定科目構成マスタ"."親科目コード" IS '親科目のコード';
COMMENT ON COLUMN "勘定科目構成マスタ"."表示順序" IS '同じ階層内での表示順序';

-- パスでの検索を高速化するためのインデックス
CREATE INDEX idx_account_structure_path
    ON "勘定科目構成マスタ" ("勘定科目パス");

-- 親科目での検索を高速化するためのインデックス（部分インデックス）
CREATE INDEX idx_account_structure_parent
    ON "勘定科目構成マスタ" ("親科目コード")
    WHERE "親科目コード" IS NOT NULL;

-- 階層レベルでの検索を高速化するためのインデックス
CREATE INDEX idx_account_structure_level
    ON "勘定科目構成マスタ" ("階層レベル");

COMMENT ON INDEX idx_account_structure_path IS 'パスでの検索を高速化';
COMMENT ON INDEX idx_account_structure_parent IS '親科目での検索を高速化（部分インデックス）';
COMMENT ON INDEX idx_account_structure_level IS '階層レベルでの検索を高速化';
