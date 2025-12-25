-- V020__create_quality_tables.sql
-- 品質管理テーブル

-- 検査判定 ENUM
CREATE TYPE "検査判定" AS ENUM ('合格', '不合格', '保留');

-- ロット種別 ENUM
CREATE TYPE "ロット種別" AS ENUM ('購入ロット', '製造ロット');

-- 欠点マスタはV009で作成済み

-- 出荷検査データ
CREATE TABLE "出荷検査データ" (
    "ID" SERIAL PRIMARY KEY,
    "出荷検査番号" VARCHAR(20) UNIQUE NOT NULL,
    "出荷番号" VARCHAR(20) NOT NULL,
    "品目コード" VARCHAR(20) NOT NULL,
    "検査日" DATE NOT NULL,
    "検査担当者コード" VARCHAR(20) NOT NULL,
    "検査数量" DECIMAL(15, 2) NOT NULL,
    "合格数" DECIMAL(15, 2) NOT NULL,
    "不合格数" DECIMAL(15, 2) NOT NULL,
    "判定" "検査判定" NOT NULL,
    "作成日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "更新日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL
);

COMMENT ON TABLE "出荷検査データ" IS '出荷検査データ';
COMMENT ON COLUMN "出荷検査データ"."出荷検査番号" IS '出荷検査番号';
COMMENT ON COLUMN "出荷検査データ"."出荷番号" IS '出荷番号';
COMMENT ON COLUMN "出荷検査データ"."判定" IS '検査判定';

-- 出荷検査結果データ
CREATE TABLE "出荷検査結果データ" (
    "ID" SERIAL PRIMARY KEY,
    "出荷検査番号" VARCHAR(20) NOT NULL,
    "欠点コード" VARCHAR(20) NOT NULL,
    "数量" DECIMAL(15, 2) NOT NULL,
    "作成日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "更新日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT "UK_出荷検査結果" UNIQUE ("出荷検査番号", "欠点コード"),
    CONSTRAINT "FK_出荷検査結果_出荷検査" FOREIGN KEY ("出荷検査番号")
        REFERENCES "出荷検査データ"("出荷検査番号"),
    CONSTRAINT "FK_出荷検査結果_欠点" FOREIGN KEY ("欠点コード")
        REFERENCES "欠点マスタ"("欠点コード")
);

COMMENT ON TABLE "出荷検査結果データ" IS '出荷検査結果データ';

-- ロットマスタ
CREATE TABLE "ロットマスタ" (
    "ID" SERIAL PRIMARY KEY,
    "ロット番号" VARCHAR(30) UNIQUE NOT NULL,
    "品目コード" VARCHAR(20) NOT NULL,
    "ロット種別" "ロット種別" NOT NULL,
    "製造日" DATE,
    "有効期限" DATE,
    "数量" DECIMAL(15, 2) NOT NULL,
    "作成日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "更新日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL
);

COMMENT ON TABLE "ロットマスタ" IS 'ロットマスタ';
COMMENT ON COLUMN "ロットマスタ"."ロット番号" IS 'ロット番号';
COMMENT ON COLUMN "ロットマスタ"."ロット種別" IS 'ロット種別（購入/製造）';

-- ロット構成
CREATE TABLE "ロット構成" (
    "ID" SERIAL PRIMARY KEY,
    "親ロット番号" VARCHAR(30) NOT NULL,
    "子ロット番号" VARCHAR(30) NOT NULL,
    "使用数量" DECIMAL(15, 2) NOT NULL,
    "作成日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "更新日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT "UK_ロット構成" UNIQUE ("親ロット番号", "子ロット番号"),
    CONSTRAINT "FK_ロット構成_親" FOREIGN KEY ("親ロット番号")
        REFERENCES "ロットマスタ"("ロット番号"),
    CONSTRAINT "FK_ロット構成_子" FOREIGN KEY ("子ロット番号")
        REFERENCES "ロットマスタ"("ロット番号")
);

COMMENT ON TABLE "ロット構成" IS 'ロット構成（トレーサビリティ用）';

CREATE INDEX "IDX_出荷検査_品目" ON "出荷検査データ" ("品目コード");
CREATE INDEX "IDX_出荷検査_出荷番号" ON "出荷検査データ" ("出荷番号");
CREATE INDEX "IDX_ロット_品目" ON "ロットマスタ" ("品目コード");
CREATE INDEX "IDX_ロット構成_親" ON "ロット構成" ("親ロット番号");
CREATE INDEX "IDX_ロット構成_子" ON "ロット構成" ("子ロット番号");
