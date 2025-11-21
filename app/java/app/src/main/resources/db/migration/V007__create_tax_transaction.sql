-- 課税取引マスタ
CREATE TABLE IF NOT EXISTS "課税取引マスタ" (
    "課税取引コード" VARCHAR(2) PRIMARY KEY,
    "課税取引名" VARCHAR(20) NOT NULL,
    "税率" DECIMAL(5, 3) NOT NULL DEFAULT 0.000,
    "説明" VARCHAR(200),
    "有効フラグ" BOOLEAN NOT NULL DEFAULT true,
    "作成日時" TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "更新日時" TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT check_tax_rate CHECK ("税率" >= 0 AND "税率" <= 1)
);

COMMENT ON TABLE "課税取引マスタ" IS '消費税の課税取引区分を管理するマスタテーブル';
COMMENT ON COLUMN "課税取引マスタ"."課税取引コード" IS '課税取引コード（01:課税、02:非課税、03:免税、04:不課税）';
COMMENT ON COLUMN "課税取引マスタ"."課税取引名" IS '課税取引名';
COMMENT ON COLUMN "課税取引マスタ"."税率" IS '適用される税率（0.10 = 10%）';
COMMENT ON COLUMN "課税取引マスタ"."説明" IS '課税取引の説明';
COMMENT ON COLUMN "課税取引マスタ"."有効フラグ" IS '有効な課税取引区分かどうか';

-- 初期データ投入
INSERT INTO "課税取引マスタ" ("課税取引コード", "課税取引名", "税率", "説明") VALUES
    ('01', '課税', 0.10, '消費税が課税される取引'),
    ('02', '非課税', 0.000, '消費税が非課税の取引（土地の譲渡、住宅の貸付など）'),
    ('03', '免税', 0.000, '消費税が免税の取引（輸出取引など）'),
    ('04', '不課税', 0.000, '消費税の対象外の取引（給与、配当など）');

-- 勘定科目マスタに課税取引コード列を追加
ALTER TABLE "勘定科目マスタ"
    ADD COLUMN "課税取引コード" VARCHAR(2);

COMMENT ON COLUMN "勘定科目マスタ"."課税取引コード" IS '課税取引区分コード';

-- 勘定科目マスタに外部キー制約を追加
ALTER TABLE "勘定科目マスタ"
    ADD CONSTRAINT fk_account_tax_code
    FOREIGN KEY ("課税取引コード")
    REFERENCES "課税取引マスタ"("課税取引コード")
    ON DELETE SET NULL;
