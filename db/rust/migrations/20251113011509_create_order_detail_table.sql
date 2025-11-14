-- 受注データ明細テーブルの作成

CREATE TABLE "受注データ明細" (
    "受注番号" VARCHAR(10) NOT NULL,
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
    "作成日時" TIMESTAMP(6) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "作成者名" VARCHAR(12),
    "更新日時" TIMESTAMP(6) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "更新者名" VARCHAR(12),

    CONSTRAINT pk_order_detail PRIMARY KEY ("受注番号", "明細番号"),

    -- 受注データへの外部キー制約
    CONSTRAINT fk_order_detail_order
        FOREIGN KEY ("受注番号")
        REFERENCES "受注データ" ("受注番号")
        ON DELETE CASCADE
        ON UPDATE CASCADE,

    -- 商品マスタへの外部キー制約
    CONSTRAINT fk_order_detail_product
        FOREIGN KEY ("商品コード")
        REFERENCES "商品マスタ" ("商品コード")
        ON DELETE RESTRICT
        ON UPDATE CASCADE
);

CREATE INDEX idx_order_detail_prod_code ON "受注データ明細" ("商品コード");

COMMENT ON TABLE "受注データ明細" IS '受注の明細情報（商品ごと）を記録するテーブル';
COMMENT ON COLUMN "受注データ明細"."受注番号" IS '親の受注番号（外部キー）';
COMMENT ON COLUMN "受注データ明細"."明細番号" IS '明細の連番';
COMMENT ON COLUMN "受注データ明細"."商品コード" IS '受注した商品（外部キー）';
COMMENT ON COLUMN "受注データ明細"."数量" IS '受注数量';
COMMENT ON COLUMN "受注データ明細"."単価" IS '受注時の販売単価';
