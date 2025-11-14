-- 倉庫マスタテーブルの作成

CREATE TABLE "倉庫マスタ" (
    "倉庫コード" VARCHAR(3) NOT NULL,
    "倉庫名" VARCHAR(40),
    "倉庫名略称" VARCHAR(20),
    "郵便番号" CHAR(8),
    "都道府県" VARCHAR(4),
    "住所１" VARCHAR(40),
    "住所２" VARCHAR(40),
    "電話番号" VARCHAR(13),
    "ＦＡＸ番号" VARCHAR(13),
    "作成日時" TIMESTAMP(6) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "作成者名" VARCHAR(12),
    "更新日時" TIMESTAMP(6) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "更新者名" VARCHAR(12),

    CONSTRAINT pk_warehouse PRIMARY KEY ("倉庫コード")
);

CREATE INDEX idx_warehouse_name ON "倉庫マスタ" ("倉庫名");

COMMENT ON TABLE "倉庫マスタ" IS '商品を保管する倉庫の情報を管理するマスタテーブル';
COMMENT ON COLUMN "倉庫マスタ"."倉庫コード" IS '倉庫を一意に識別するコード';
COMMENT ON COLUMN "倉庫マスタ"."倉庫名" IS '倉庫の正式名称';
COMMENT ON COLUMN "倉庫マスタ"."倉庫名略称" IS '倉庫の略称';
COMMENT ON COLUMN "倉庫マスタ"."郵便番号" IS '倉庫の郵便番号';
COMMENT ON COLUMN "倉庫マスタ"."都道府県" IS '倉庫の所在都道府県';
COMMENT ON COLUMN "倉庫マスタ"."住所１" IS '倉庫の住所（市区町村）';
COMMENT ON COLUMN "倉庫マスタ"."住所２" IS '倉庫の住所（番地以降）';
COMMENT ON COLUMN "倉庫マスタ"."電話番号" IS '倉庫の電話番号';
COMMENT ON COLUMN "倉庫マスタ"."ＦＡＸ番号" IS '倉庫のFAX番号';
