-- 商品マスタテーブルの作成

CREATE TABLE "商品マスタ" (
    "商品コード" VARCHAR(16) NOT NULL,
    "商品正式名" VARCHAR(40) NOT NULL,
    "商品略称" VARCHAR(10) NOT NULL,
    "商品名カナ" VARCHAR(20) NOT NULL,
    "商品区分" VARCHAR(1),
    "製品型番" VARCHAR(40),
    "販売単価" INTEGER NOT NULL DEFAULT 0,
    "仕入単価" INTEGER DEFAULT 0,
    "売上原価" INTEGER NOT NULL DEFAULT 0,
    "税区分" INTEGER NOT NULL DEFAULT 1,
    "商品分類コード" VARCHAR(8),
    "雑区分" INTEGER,
    "在庫管理対象区分" INTEGER DEFAULT 1,
    "在庫引当区分" INTEGER,
    "仕入先コード" VARCHAR(8) NOT NULL,
    "仕入先枝番" INTEGER,
    "作成日時" TIMESTAMP(6) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "作成者名" VARCHAR(12),
    "更新日時" TIMESTAMP(6) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "更新者名" VARCHAR(12),

    CONSTRAINT pk_products PRIMARY KEY ("商品コード"),

    -- 商品分類マスタへの外部キー制約
    CONSTRAINT fk_product_category
        FOREIGN KEY ("商品分類コード")
        REFERENCES "商品分類マスタ" ("商品分類コード")
        ON DELETE RESTRICT
        ON UPDATE CASCADE
);

-- インデックスの作成
CREATE INDEX idx_product_category_code ON "商品マスタ" ("商品分類コード");
CREATE INDEX idx_product_name ON "商品マスタ" ("商品略称");
CREATE INDEX idx_product_kana ON "商品マスタ" ("商品名カナ");

-- テーブルとカラムにコメントを追加
COMMENT ON TABLE "商品マスタ" IS '商品情報を管理するマスタテーブル';
COMMENT ON COLUMN "商品マスタ"."商品コード" IS '商品を一意に識別するコード';
COMMENT ON COLUMN "商品マスタ"."商品正式名" IS '商品の正式名称';
COMMENT ON COLUMN "商品マスタ"."商品略称" IS '商品の略称';
COMMENT ON COLUMN "商品マスタ"."商品名カナ" IS '商品名の読み仮名';
COMMENT ON COLUMN "商品マスタ"."商品区分" IS '商品の区分（1:製品、2:商品、3:サービスなど）';
COMMENT ON COLUMN "商品マスタ"."製品型番" IS 'メーカーの型番';
COMMENT ON COLUMN "商品マスタ"."販売単価" IS '標準販売単価';
COMMENT ON COLUMN "商品マスタ"."仕入単価" IS '標準仕入単価';
COMMENT ON COLUMN "商品マスタ"."売上原価" IS '原価';
COMMENT ON COLUMN "商品マスタ"."税区分" IS '消費税区分（1:課税、2:非課税など）';
COMMENT ON COLUMN "商品マスタ"."商品分類コード" IS '所属する商品分類（外部キー）';
COMMENT ON COLUMN "商品マスタ"."在庫管理対象区分" IS '在庫管理の対象かどうか';
COMMENT ON COLUMN "商品マスタ"."在庫引当区分" IS '在庫引当の方法';
COMMENT ON COLUMN "商品マスタ"."仕入先コード" IS '主要仕入先のコード';
