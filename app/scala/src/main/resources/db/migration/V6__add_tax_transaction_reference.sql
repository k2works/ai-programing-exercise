-- 勘定科目マスタに課税取引コードを追加

-- 課税取引コードカラムを追加
ALTER TABLE "勘定科目マスタ"
    ADD COLUMN "課税取引コード" VARCHAR(2);

-- 外部キー制約を追加（課税取引マスタは後で作成するため、まだ設定しない）
-- 将来的には以下のような外部キー制約を追加する予定:
-- ALTER TABLE "勘定科目マスタ"
--     ADD CONSTRAINT fk_account_tax
--     FOREIGN KEY ("課税取引コード")
--     REFERENCES "課税取引マスタ" ("課税取引コード");

COMMENT ON COLUMN "勘定科目マスタ"."課税取引コード" IS
    '課税取引コード（課税取引マスタへの外部キー）';

-- インデックスを追加
CREATE INDEX IF NOT EXISTS idx_account_tax_code
    ON "勘定科目マスタ" ("課税取引コード")
    WHERE "課税取引コード" IS NOT NULL;
