-- 代替商品テーブルの作成

CREATE TABLE "代替商品" (
    "商品コード" VARCHAR(16) NOT NULL,
    "代替商品コード" VARCHAR(16) NOT NULL,
    "優先順位" INTEGER DEFAULT 1,
    "作成日時" TIMESTAMP(6) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "作成者名" VARCHAR(12),
    "更新日時" TIMESTAMP(6) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "更新者名" VARCHAR(12),

    CONSTRAINT pk_alternate_products PRIMARY KEY ("商品コード", "代替商品コード"),

    -- 商品マスタへの外部キー制約
    CONSTRAINT fk_alternate_product_product
        FOREIGN KEY ("商品コード")
        REFERENCES "商品マスタ" ("商品コード")
        ON DELETE CASCADE
        ON UPDATE CASCADE
);

-- インデックスの作成
CREATE INDEX idx_alternate_product_alt_code ON "代替商品" ("代替商品コード");

-- テーブルとカラムにコメントを追加
COMMENT ON TABLE "代替商品" IS '商品の代替品を管理するテーブル';
COMMENT ON COLUMN "代替商品"."商品コード" IS '元の商品コード（外部キー）';
COMMENT ON COLUMN "代替商品"."代替商品コード" IS '代替可能な商品のコード';
COMMENT ON COLUMN "代替商品"."優先順位" IS '代替品としての優先順位（1が最優先）';
