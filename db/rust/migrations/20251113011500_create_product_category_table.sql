-- 商品分類マスタテーブルの作成

CREATE TABLE "商品分類マスタ" (
    "商品分類コード" VARCHAR(8) NOT NULL,
    "商品分類名" VARCHAR(30),
    "商品分類階層" INTEGER NOT NULL DEFAULT 0,
    "商品分類パス" VARCHAR(100),
    "最下層区分" INTEGER DEFAULT 0,
    "作成日時" TIMESTAMP(6) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "作成者名" VARCHAR(12),
    "更新日時" TIMESTAMP(6) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "更新者名" VARCHAR(12),

    CONSTRAINT pk_product_category PRIMARY KEY ("商品分類コード")
);

-- テーブルとカラムにコメントを追加
COMMENT ON TABLE "商品分類マスタ" IS '商品を階層的に分類するためのマスタテーブル';
COMMENT ON COLUMN "商品分類マスタ"."商品分類コード" IS '商品分類を一意に識別するコード';
COMMENT ON COLUMN "商品分類マスタ"."商品分類名" IS '商品分類の名称';
COMMENT ON COLUMN "商品分類マスタ"."商品分類階層" IS '分類の階層レベル（1:トップ、2:第2階層...）';
COMMENT ON COLUMN "商品分類マスタ"."商品分類パス" IS '階層構造を表すパス（例: /CAT001/CAT002）';
COMMENT ON COLUMN "商品分類マスタ"."最下層区分" IS '最下層の分類かどうか（0:上位層、1:最下層）';
