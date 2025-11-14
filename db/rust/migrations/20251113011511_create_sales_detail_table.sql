-- 売上データ明細テーブルの作成

CREATE TABLE "売上データ明細" (
    "売上番号" VARCHAR(10) NOT NULL,
    "明細番号" INTEGER NOT NULL,
    "商品コード" VARCHAR(16) NOT NULL,
    "商品名" VARCHAR(40),
    "商品名略称" VARCHAR(60),
    "色" VARCHAR(20),
    "サイズ" VARCHAR(20),
    "数量" INTEGER NOT NULL,
    "単位" VARCHAR(4),
    "単価" INTEGER DEFAULT 0,
    "売上原価" INTEGER DEFAULT 0,
    "受注番号" VARCHAR(10),
    "受注明細番号" INTEGER,
    "作成日時" TIMESTAMP(6) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "作成者名" VARCHAR(12),
    "更新日時" TIMESTAMP(6) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "更新者名" VARCHAR(12),

    CONSTRAINT pk_sales_detail PRIMARY KEY ("売上番号", "明細番号"),

    -- 売上データへの外部キー制約
    CONSTRAINT fk_sales_detail_sales
        FOREIGN KEY ("売上番号")
        REFERENCES "売上データ" ("売上番号")
        ON DELETE CASCADE
        ON UPDATE CASCADE,

    -- 商品マスタへの外部キー制約
    CONSTRAINT fk_sales_detail_product
        FOREIGN KEY ("商品コード")
        REFERENCES "商品マスタ" ("商品コード")
        ON DELETE RESTRICT
        ON UPDATE CASCADE
);

CREATE INDEX idx_sales_detail_prod_code ON "売上データ明細" ("商品コード");
CREATE INDEX idx_sales_detail_order ON "売上データ明細" ("受注番号", "受注明細番号");

COMMENT ON TABLE "売上データ明細" IS '売上の明細情報（商品ごと）を記録するテーブル';
COMMENT ON COLUMN "売上データ明細"."売上番号" IS '親の売上番号（外部キー）';
COMMENT ON COLUMN "売上データ明細"."明細番号" IS '明細の連番';
COMMENT ON COLUMN "売上データ明細"."商品コード" IS '売上商品（外部キー）';
COMMENT ON COLUMN "売上データ明細"."数量" IS '売上数量';
COMMENT ON COLUMN "売上データ明細"."単価" IS '売上時の販売単価';
COMMENT ON COLUMN "売上データ明細"."受注番号" IS '元となった受注番号（任意）';
COMMENT ON COLUMN "売上データ明細"."受注明細番号" IS '元となった受注明細番号（任意）';
