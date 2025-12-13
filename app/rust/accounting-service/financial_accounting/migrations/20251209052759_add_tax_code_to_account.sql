-- 課税取引コードカラムを追加
ALTER TABLE "勘定科目マスタ"
    ADD COLUMN "課税取引コード" VARCHAR(2);

-- コメント追加
COMMENT ON COLUMN "勘定科目マスタ"."課税取引コード" IS
    '課税取引コード（課税取引マスタへの外部キー）';

-- インデックスを追加（部分インデックス）
CREATE INDEX IF NOT EXISTS idx_account_tax_code
    ON "勘定科目マスタ" ("課税取引コード")
    WHERE "課税取引コード" IS NOT NULL;

COMMENT ON INDEX idx_account_tax_code IS
    '課税取引コードでの検索を高速化（部分インデックス）';
