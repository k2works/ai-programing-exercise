-- 発注データ明細テーブルの作成

CREATE TABLE "発注データ明細" (
    "発注番号" VARCHAR(10) NOT NULL,
    "発注行番号" INTEGER NOT NULL,
    "商品コード" VARCHAR(16) NOT NULL,
    "商品名" VARCHAR(40),
    "発注数量" INTEGER NOT NULL,
    "単価" INTEGER DEFAULT 0,
    "入荷予定数量" INTEGER DEFAULT 0,
    "入荷済数量" INTEGER DEFAULT 0,
    "完了フラグ" INTEGER NOT NULL DEFAULT 0,
    "作成日時" TIMESTAMP(6) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "作成者名" VARCHAR(12),
    "更新日時" TIMESTAMP(6) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "更新者名" VARCHAR(12),

    CONSTRAINT pk_po_detail PRIMARY KEY ("発注番号", "発注行番号"),

    -- 発注データへの外部キー制約
    CONSTRAINT fk_po_detail_po
        FOREIGN KEY ("発注番号")
        REFERENCES "発注データ" ("発注番号")
        ON DELETE CASCADE
        ON UPDATE CASCADE,

    -- 商品マスタへの外部キー制約
    CONSTRAINT fk_po_detail_product
        FOREIGN KEY ("商品コード")
        REFERENCES "商品マスタ" ("商品コード")
        ON DELETE RESTRICT
        ON UPDATE CASCADE
);

CREATE INDEX idx_po_detail_prod_code ON "発注データ明細" ("商品コード");

COMMENT ON TABLE "発注データ明細" IS '発注の明細情報（商品ごと）を記録するテーブル';
COMMENT ON COLUMN "発注データ明細"."発注番号" IS '親の発注番号（外部キー）';
COMMENT ON COLUMN "発注データ明細"."発注行番号" IS '明細の連番';
COMMENT ON COLUMN "発注データ明細"."商品コード" IS '発注した商品（外部キー）';
COMMENT ON COLUMN "発注データ明細"."発注数量" IS '発注数量';
COMMENT ON COLUMN "発注データ明細"."単価" IS '発注時の仕入単価';
COMMENT ON COLUMN "発注データ明細"."入荷予定数量" IS '入荷予定の数量';
COMMENT ON COLUMN "発注データ明細"."入荷済数量" IS '実際に入荷した数量';
COMMENT ON COLUMN "発注データ明細"."完了フラグ" IS '発注完了フラグ（0:未完了、1:完了）';
