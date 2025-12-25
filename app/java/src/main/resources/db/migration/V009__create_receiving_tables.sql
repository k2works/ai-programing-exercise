-- V009__create_receiving_tables.sql

-- 入荷受入データ
-- 品目マスタは複合主キー（コード＋適用開始日）のためFK制約なし
CREATE TABLE "入荷受入データ" (
    "ID" SERIAL PRIMARY KEY,
    "入荷番号" VARCHAR(20) UNIQUE NOT NULL,
    "発注番号" VARCHAR(20) NOT NULL,
    "発注行番号" INTEGER NOT NULL,
    "入荷日" DATE NOT NULL,
    "入荷担当者コード" VARCHAR(20),
    "入荷受入区分" 入荷受入区分 DEFAULT '通常入荷' NOT NULL,
    "品目コード" VARCHAR(20) NOT NULL,
    "諸口品目区分" BOOLEAN DEFAULT FALSE NOT NULL,
    "入荷数量" DECIMAL(15, 2) NOT NULL,
    "入荷備考" TEXT,
    "作成日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "作成者" VARCHAR(50),
    "更新日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "更新者" VARCHAR(50),
    CONSTRAINT "fk_入荷受入_発注明細"
        FOREIGN KEY ("発注番号", "発注行番号") REFERENCES "発注明細データ"("発注番号", "発注行番号")
);

-- 欠点マスタ
CREATE TABLE "欠点マスタ" (
    "ID" SERIAL PRIMARY KEY,
    "欠点コード" VARCHAR(20) UNIQUE NOT NULL,
    "欠点内容" VARCHAR(200) NOT NULL,
    "作成日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "作成者" VARCHAR(50),
    "更新日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "更新者" VARCHAR(50)
);

-- 受入検査データ
CREATE TABLE "受入検査データ" (
    "ID" SERIAL PRIMARY KEY,
    "受入検査番号" VARCHAR(20) UNIQUE NOT NULL,
    "入荷番号" VARCHAR(20) NOT NULL,
    "発注番号" VARCHAR(20) NOT NULL,
    "発注行番号" INTEGER NOT NULL,
    "受入検査日" DATE NOT NULL,
    "受入検査担当者コード" VARCHAR(20),
    "品目コード" VARCHAR(20) NOT NULL,
    "諸口品目区分" BOOLEAN DEFAULT FALSE NOT NULL,
    "良品数" DECIMAL(15, 2) NOT NULL,
    "不良品数" DECIMAL(15, 2) DEFAULT 0 NOT NULL,
    "受入検査備考" TEXT,
    "作成日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "作成者" VARCHAR(50),
    "更新日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "更新者" VARCHAR(50),
    CONSTRAINT "fk_受入検査_入荷"
        FOREIGN KEY ("入荷番号") REFERENCES "入荷受入データ"("入荷番号")
);

-- 検収データ
-- 取引先マスタは複合主キー（コード＋適用開始日）のためFK制約なし
CREATE TABLE "検収データ" (
    "ID" SERIAL PRIMARY KEY,
    "検収番号" VARCHAR(20) UNIQUE NOT NULL,
    "受入検査番号" VARCHAR(20) NOT NULL,
    "発注番号" VARCHAR(20) NOT NULL,
    "発注行番号" INTEGER NOT NULL,
    "検収日" DATE NOT NULL,
    "検収担当者コード" VARCHAR(20),
    "取引先コード" VARCHAR(20) NOT NULL,
    "品目コード" VARCHAR(20) NOT NULL,
    "諸口品目区分" BOOLEAN DEFAULT FALSE NOT NULL,
    "検収数" DECIMAL(15, 2) NOT NULL,
    "検収単価" DECIMAL(15, 2) NOT NULL,
    "検収金額" DECIMAL(15, 2) NOT NULL,
    "検収消費税額" DECIMAL(15, 2) DEFAULT 0 NOT NULL,
    "検収備考" TEXT,
    "作成日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "作成者" VARCHAR(50),
    "更新日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "更新者" VARCHAR(50),
    CONSTRAINT "fk_検収_受入検査"
        FOREIGN KEY ("受入検査番号") REFERENCES "受入検査データ"("受入検査番号"),
    CONSTRAINT "fk_検収_発注明細"
        FOREIGN KEY ("発注番号", "発注行番号") REFERENCES "発注明細データ"("発注番号", "発注行番号")
);

-- インデックス
CREATE INDEX "idx_入荷受入_発注番号" ON "入荷受入データ"("発注番号", "発注行番号");
CREATE INDEX "idx_入荷受入_入荷日" ON "入荷受入データ"("入荷日");
CREATE INDEX "idx_受入検査_入荷番号" ON "受入検査データ"("入荷番号");
CREATE INDEX "idx_検収_受入検査番号" ON "検収データ"("受入検査番号");
CREATE INDEX "idx_検収_発注番号" ON "検収データ"("発注番号", "発注行番号");
