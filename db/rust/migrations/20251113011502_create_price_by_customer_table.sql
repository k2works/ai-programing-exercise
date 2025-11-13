-- 顧客別販売単価テーブルの作成

CREATE TABLE "顧客別販売単価" (
    "商品コード" VARCHAR(16) NOT NULL,
    "取引先コード" VARCHAR(8) NOT NULL,
    "販売単価" INTEGER NOT NULL DEFAULT 0,
    "作成日時" TIMESTAMP(6) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "作成者名" VARCHAR(12),
    "更新日時" TIMESTAMP(6) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "更新者名" VARCHAR(12),

    CONSTRAINT pk_pricebycustomer PRIMARY KEY ("商品コード", "取引先コード"),

    -- 商品マスタへの外部キー制約
    CONSTRAINT fk_pricebycustomer_product
        FOREIGN KEY ("商品コード")
        REFERENCES "商品マスタ" ("商品コード")
        ON DELETE CASCADE
        ON UPDATE CASCADE
);

-- インデックスの作成
CREATE INDEX idx_price_by_customer_comp_code ON "顧客別販売単価" ("取引先コード");

-- テーブルとカラムにコメントを追加
COMMENT ON TABLE "顧客別販売単価" IS '顧客ごとに異なる販売単価を設定するテーブル';
COMMENT ON COLUMN "顧客別販売単価"."商品コード" IS '商品コード（外部キー）';
COMMENT ON COLUMN "顧客別販売単価"."取引先コード" IS '取引先コード';
COMMENT ON COLUMN "顧客別販売単価"."販売単価" IS 'この顧客向けの特別単価';
