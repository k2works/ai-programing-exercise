-- V019__create_stocktaking_tables.sql
-- 棚卸業務テーブル

-- 棚卸ステータス ENUM
CREATE TYPE "棚卸ステータス" AS ENUM ('発行済', '入力済', '確定');

-- 棚卸データ
CREATE TABLE "棚卸データ" (
    "ID" SERIAL PRIMARY KEY,
    "棚卸番号" VARCHAR(20) UNIQUE NOT NULL,
    "場所コード" VARCHAR(20) NOT NULL,
    "棚卸日" DATE NOT NULL,
    "ステータス" "棚卸ステータス" DEFAULT '発行済' NOT NULL,
    "作成日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "更新日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT "FK_棚卸_場所" FOREIGN KEY ("場所コード")
        REFERENCES "場所マスタ"("場所コード")
);

COMMENT ON TABLE "棚卸データ" IS '棚卸データ';
COMMENT ON COLUMN "棚卸データ"."棚卸番号" IS '棚卸番号';
COMMENT ON COLUMN "棚卸データ"."場所コード" IS '場所コード';
COMMENT ON COLUMN "棚卸データ"."棚卸日" IS '棚卸日';
COMMENT ON COLUMN "棚卸データ"."ステータス" IS '棚卸ステータス';

-- 棚卸明細データ
CREATE TABLE "棚卸明細データ" (
    "ID" SERIAL PRIMARY KEY,
    "棚卸番号" VARCHAR(20) NOT NULL,
    "棚卸行番号" INT NOT NULL,
    "品目コード" VARCHAR(20) NOT NULL,
    "帳簿数量" DECIMAL(15, 2) NOT NULL,
    "実棚数量" DECIMAL(15, 2),
    "差異数量" DECIMAL(15, 2),
    "作成日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "更新日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT "UK_棚卸明細" UNIQUE ("棚卸番号", "棚卸行番号"),
    CONSTRAINT "FK_棚卸明細_棚卸" FOREIGN KEY ("棚卸番号")
        REFERENCES "棚卸データ"("棚卸番号")
);

COMMENT ON TABLE "棚卸明細データ" IS '棚卸明細データ';

-- 在庫調整データ
CREATE TABLE "在庫調整データ" (
    "ID" SERIAL PRIMARY KEY,
    "在庫調整番号" VARCHAR(30) UNIQUE NOT NULL,
    "棚卸番号" VARCHAR(20),
    "品目コード" VARCHAR(20) NOT NULL,
    "場所コード" VARCHAR(20) NOT NULL,
    "調整日" DATE NOT NULL,
    "調整担当者コード" VARCHAR(20) NOT NULL,
    "調整数" DECIMAL(15, 2) NOT NULL,
    "理由コード" VARCHAR(20) NOT NULL,
    "作成日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "更新日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT "FK_在庫調整_棚卸" FOREIGN KEY ("棚卸番号")
        REFERENCES "棚卸データ"("棚卸番号"),
    CONSTRAINT "FK_在庫調整_場所" FOREIGN KEY ("場所コード")
        REFERENCES "場所マスタ"("場所コード")
);

COMMENT ON TABLE "在庫調整データ" IS '在庫調整データ';
COMMENT ON COLUMN "在庫調整データ"."在庫調整番号" IS '在庫調整番号';
COMMENT ON COLUMN "在庫調整データ"."調整数" IS '調整数（+/-）';
COMMENT ON COLUMN "在庫調整データ"."理由コード" IS '調整理由コード';

CREATE INDEX "IDX_棚卸_場所" ON "棚卸データ" ("場所コード");
CREATE INDEX "IDX_棚卸_日付" ON "棚卸データ" ("棚卸日");
CREATE INDEX "IDX_棚卸明細_品目" ON "棚卸明細データ" ("品目コード");
CREATE INDEX "IDX_在庫調整_棚卸" ON "在庫調整データ" ("棚卸番号");
CREATE INDEX "IDX_在庫調整_品目" ON "在庫調整データ" ("品目コード");
