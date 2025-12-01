-- 課税取引マスタ
CREATE TABLE IF NOT EXISTS "課税取引マスタ" (
    "課税取引コード" VARCHAR(2) PRIMARY KEY,
    "課税取引名" VARCHAR(20) NOT NULL,
    "税率" DECIMAL(5,3) NOT NULL,
    "作成日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "更新日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,

    -- CHECK制約
    CONSTRAINT chk_tax_rate
        CHECK ("税率" >= 0 AND "税率" <= 1)
);

-- テーブルコメント
COMMENT ON TABLE "課税取引マスタ" IS
    '課税取引マスタ（消費税課税区分を管理）';

-- カラムコメント
COMMENT ON COLUMN "課税取引マスタ"."課税取引コード" IS '課税取引コード（主キー）';
COMMENT ON COLUMN "課税取引マスタ"."課税取引名" IS '課税取引名';
COMMENT ON COLUMN "課税取引マスタ"."税率" IS '税率（0.00-1.00）';

-- 初期データ投入
INSERT INTO "課税取引マスタ" ("課税取引コード", "課税取引名", "税率") VALUES
    ('01', '課税売上（標準税率）', 0.100),
    ('02', '課税売上（軽減税率）', 0.080),
    ('03', '非課税売上', 0.000),
    ('04', '免税売上', 0.000),
    ('05', '不課税取引', 0.000)
ON CONFLICT ("課税取引コード") DO NOTHING;

-- 勘定科目マスタとの外部キー制約を追加
ALTER TABLE "勘定科目マスタ"
    ADD CONSTRAINT fk_account_tax
    FOREIGN KEY ("課税取引コード")
    REFERENCES "課税取引マスタ" ("課税取引コード");
