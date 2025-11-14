-- 仕入データ明細テーブルの作成

CREATE TABLE "仕入データ明細" (
    "仕入番号" VARCHAR(10) NOT NULL,
    "仕入行番号" INTEGER NOT NULL,
    "商品コード" VARCHAR(16) NOT NULL,
    "商品名" VARCHAR(40),
    "仕入数量" INTEGER NOT NULL,
    "単価" INTEGER DEFAULT 0,
    "倉庫コード" VARCHAR(3) NOT NULL,
    "ロット番号" VARCHAR(20) NOT NULL,
    "発注番号" VARCHAR(10),
    "発注行番号" INTEGER,
    "作成日時" TIMESTAMP(6) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "作成者名" VARCHAR(12),
    "更新日時" TIMESTAMP(6) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "更新者名" VARCHAR(12),

    CONSTRAINT pk_purchase_detail PRIMARY KEY ("仕入番号", "仕入行番号"),

    -- 仕入データへの外部キー制約
    CONSTRAINT fk_purchase_detail_purchase
        FOREIGN KEY ("仕入番号")
        REFERENCES "仕入データ" ("仕入番号")
        ON DELETE CASCADE
        ON UPDATE CASCADE,

    -- 商品マスタへの外部キー制約
    CONSTRAINT fk_purchase_detail_product
        FOREIGN KEY ("商品コード")
        REFERENCES "商品マスタ" ("商品コード")
        ON DELETE RESTRICT
        ON UPDATE CASCADE,

    -- 倉庫マスタへの外部キー制約
    CONSTRAINT fk_purchase_detail_warehouse
        FOREIGN KEY ("倉庫コード")
        REFERENCES "倉庫マスタ" ("倉庫コード")
        ON DELETE RESTRICT
        ON UPDATE CASCADE
);

CREATE INDEX idx_purchase_detail_prod_code ON "仕入データ明細" ("商品コード");
CREATE INDEX idx_purchase_detail_wh_code ON "仕入データ明細" ("倉庫コード");
CREATE INDEX idx_purchase_detail_po ON "仕入データ明細" ("発注番号", "発注行番号");

COMMENT ON TABLE "仕入データ明細" IS '仕入の明細情報（商品ごと）を記録するテーブル';
COMMENT ON COLUMN "仕入データ明細"."仕入番号" IS '親の仕入番号（外部キー）';
COMMENT ON COLUMN "仕入データ明細"."仕入行番号" IS '明細の連番';
COMMENT ON COLUMN "仕入データ明細"."商品コード" IS '仕入した商品（外部キー）';
COMMENT ON COLUMN "仕入データ明細"."仕入数量" IS '仕入数量';
COMMENT ON COLUMN "仕入データ明細"."単価" IS '仕入時の仕入単価';
COMMENT ON COLUMN "仕入データ明細"."倉庫コード" IS '入荷先の倉庫（外部キー）';
COMMENT ON COLUMN "仕入データ明細"."ロット番号" IS '仕入商品のロット番号';
COMMENT ON COLUMN "仕入データ明細"."発注番号" IS '元となった発注番号（任意）';
COMMENT ON COLUMN "仕入データ明細"."発注行番号" IS '元となった発注明細番号（任意）';
