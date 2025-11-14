-- 在庫データテーブルの作成

CREATE TABLE "在庫データ" (
    "倉庫コード" VARCHAR(3) NOT NULL,
    "商品コード" VARCHAR(16) NOT NULL,
    "ロット番号" VARCHAR(20) NOT NULL,
    "在庫区分" VARCHAR(1) NOT NULL DEFAULT '1',
    "良品区分" VARCHAR(1) NOT NULL DEFAULT 'G',
    "実在庫数" INTEGER NOT NULL DEFAULT 0,
    "有効在庫数" INTEGER NOT NULL DEFAULT 0,
    "最終出荷日" TIMESTAMP(6),
    "作成日時" TIMESTAMP(6) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "作成者名" VARCHAR(12),
    "更新日時" TIMESTAMP(6) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "更新者名" VARCHAR(12),

    CONSTRAINT pk_stock PRIMARY KEY ("倉庫コード", "商品コード", "ロット番号", "在庫区分", "良品区分"),

    -- 倉庫マスタへの外部キー制約
    CONSTRAINT fk_stock_warehouse
        FOREIGN KEY ("倉庫コード")
        REFERENCES "倉庫マスタ" ("倉庫コード")
        ON DELETE RESTRICT
        ON UPDATE CASCADE,

    -- 商品マスタへの外部キー制約
    CONSTRAINT fk_stock_product
        FOREIGN KEY ("商品コード")
        REFERENCES "商品マスタ" ("商品コード")
        ON DELETE RESTRICT
        ON UPDATE CASCADE
);

CREATE INDEX idx_stock_wh_code ON "在庫データ" ("倉庫コード");
CREATE INDEX idx_stock_prod_code ON "在庫データ" ("商品コード");
CREATE INDEX idx_stock_lot_no ON "在庫データ" ("ロット番号");

COMMENT ON TABLE "在庫データ" IS '倉庫ごと・商品ごと・ロットごとの在庫数を管理するテーブル';
COMMENT ON COLUMN "在庫データ"."倉庫コード" IS '在庫が保管されている倉庫（外部キー）';
COMMENT ON COLUMN "在庫データ"."商品コード" IS '在庫の商品（外部キー）';
COMMENT ON COLUMN "在庫データ"."ロット番号" IS '仕入れ時期や製造番号などを識別するロット番号';
COMMENT ON COLUMN "在庫データ"."在庫区分" IS '在庫の種類（1:通常在庫、2:不良在庫など）';
COMMENT ON COLUMN "在庫データ"."良品区分" IS '品質区分（G:良品、D:不良品など）';
COMMENT ON COLUMN "在庫データ"."実在庫数" IS '物理的に存在する在庫数';
COMMENT ON COLUMN "在庫データ"."有効在庫数" IS '販売可能な在庫数（実在庫数 - 引当済数量）';
COMMENT ON COLUMN "在庫データ"."最終出荷日" IS '最後に出荷された日付';
