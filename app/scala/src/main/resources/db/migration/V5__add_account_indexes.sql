-- 勘定科目マスタにインデックスを追加

-- 勘定科目種別のインデックス（種別での検索を高速化）
CREATE INDEX IF NOT EXISTS idx_account_type
    ON "勘定科目マスタ" ("勘定科目種別");

-- BSPL区分のインデックス（財務諸表生成時の検索を高速化）
CREATE INDEX IF NOT EXISTS idx_bspl_distinction
    ON "勘定科目マスタ" ("BSPL区分")
    WHERE "BSPL区分" IS NOT NULL;

-- 取引要素区分のインデックス
CREATE INDEX IF NOT EXISTS idx_transaction_element
    ON "勘定科目マスタ" ("取引要素区分")
    WHERE "取引要素区分" IS NOT NULL;

-- 複合インデックス（BSPL区分 + 表示順序）
-- 財務諸表での科目一覧取得を高速化
CREATE INDEX IF NOT EXISTS idx_bspl_display_order
    ON "勘定科目マスタ" ("BSPL区分", "表示順序")
    WHERE "BSPL区分" IS NOT NULL AND "表示順序" IS NOT NULL;

-- 勘定科目カナのインデックス（カナ検索を高速化）
CREATE INDEX IF NOT EXISTS idx_account_kana
    ON "勘定科目マスタ" ("勘定科目カナ")
    WHERE "勘定科目カナ" IS NOT NULL;
