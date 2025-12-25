-- V010__create_supply_tables.sql

-- 支給区分
CREATE TYPE 支給区分 AS ENUM ('有償支給', '無償支給');

-- 支給データ
-- 取引先マスタは複合主キー（コード＋適用開始日）のためFK制約なし
CREATE TABLE "支給データ" (
    "ID" SERIAL PRIMARY KEY,
    "支給番号" VARCHAR(20) UNIQUE NOT NULL,
    "発注番号" VARCHAR(20) NOT NULL,
    "発注行番号" INTEGER NOT NULL,
    "取引先コード" VARCHAR(20) NOT NULL,
    "支給日" DATE NOT NULL,
    "支給担当者コード" VARCHAR(20),
    "支給区分" 支給区分 DEFAULT '無償支給' NOT NULL,
    "備考" TEXT,
    "作成日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "作成者" VARCHAR(50),
    "更新日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "更新者" VARCHAR(50),
    CONSTRAINT "fk_支給データ_発注明細"
        FOREIGN KEY ("発注番号", "発注行番号") REFERENCES "発注明細データ"("発注番号", "発注行番号")
);

-- 支給明細データ
-- 品目マスタは複合主キー（コード＋適用開始日）のためFK制約なし
CREATE TABLE "支給明細データ" (
    "ID" SERIAL PRIMARY KEY,
    "支給番号" VARCHAR(20) NOT NULL,
    "支給行番号" INTEGER NOT NULL,
    "品目コード" VARCHAR(20) NOT NULL,
    "支給数" DECIMAL(15, 2) NOT NULL,
    "支給単価" DECIMAL(15, 2) NOT NULL,
    "支給金額" DECIMAL(15, 2) NOT NULL,
    "備考" TEXT,
    "作成日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "更新日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT "fk_支給明細_支給"
        FOREIGN KEY ("支給番号") REFERENCES "支給データ"("支給番号"),
    UNIQUE ("支給番号", "支給行番号")
);

-- インデックス
CREATE INDEX "idx_支給データ_発注番号" ON "支給データ"("発注番号", "発注行番号");
CREATE INDEX "idx_支給データ_取引先コード" ON "支給データ"("取引先コード");
CREATE INDEX "idx_支給データ_支給日" ON "支給データ"("支給日");
CREATE INDEX "idx_支給明細_品目コード" ON "支給明細データ"("品目コード");
