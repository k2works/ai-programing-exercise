-- V008__create_purchasing_tables.sql

-- 発注ステータス
CREATE TYPE 発注ステータス AS ENUM ('作成中', '発注済', '一部入荷', '入荷完了', '検収完了', '取消');

-- 入荷受入区分
CREATE TYPE 入荷受入区分 AS ENUM ('通常入荷', '分割入荷', '返品入荷');

-- 単価マスタ
-- 品目マスタ・取引先マスタは複合主キー（コード＋適用開始日）のためFK制約なし
CREATE TABLE "単価マスタ" (
    "ID" SERIAL PRIMARY KEY,
    "品目コード" VARCHAR(20) NOT NULL,
    "取引先コード" VARCHAR(20) NOT NULL,
    "ロット単位数" DECIMAL(15, 2) DEFAULT 1 NOT NULL,
    "使用開始日" DATE NOT NULL,
    "使用停止日" DATE,
    "単価" DECIMAL(15, 2) NOT NULL,
    "作成日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "作成者" VARCHAR(50),
    "更新日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "更新者" VARCHAR(50),
    UNIQUE ("品目コード", "取引先コード", "ロット単位数", "使用開始日")
);

-- 発注データ
-- 取引先マスタは複合主キー（コード＋適用開始日）のためFK制約なし
CREATE TABLE "発注データ" (
    "ID" SERIAL PRIMARY KEY,
    "発注番号" VARCHAR(20) UNIQUE NOT NULL,
    "発注日" DATE NOT NULL,
    "取引先コード" VARCHAR(20) NOT NULL,
    "発注担当者コード" VARCHAR(20),
    "発注部門コード" VARCHAR(20),
    "ステータス" 発注ステータス DEFAULT '作成中' NOT NULL,
    "備考" TEXT,
    "作成日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "作成者" VARCHAR(50),
    "更新日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "更新者" VARCHAR(50)
);

-- 発注明細データ
CREATE TABLE "発注明細データ" (
    "ID" SERIAL PRIMARY KEY,
    "発注番号" VARCHAR(20) NOT NULL,
    "発注行番号" INTEGER NOT NULL,
    "オーダNO" VARCHAR(20),
    "納入場所コード" VARCHAR(20),
    "品目コード" VARCHAR(20) NOT NULL,
    "諸口品目区分" BOOLEAN DEFAULT FALSE NOT NULL,
    "受入予定日" DATE NOT NULL,
    "回答納期" DATE,
    "発注単価" DECIMAL(15, 2) NOT NULL,
    "発注数量" DECIMAL(15, 2) NOT NULL,
    "入荷済数量" DECIMAL(15, 2) DEFAULT 0 NOT NULL,
    "検査済数量" DECIMAL(15, 2) DEFAULT 0 NOT NULL,
    "検収済数量" DECIMAL(15, 2) DEFAULT 0 NOT NULL,
    "発注金額" DECIMAL(15, 2) NOT NULL,
    "消費税金額" DECIMAL(15, 2) DEFAULT 0 NOT NULL,
    "完了フラグ" BOOLEAN DEFAULT FALSE NOT NULL,
    "明細備考" TEXT,
    "作成日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "作成者" VARCHAR(50),
    "更新日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "更新者" VARCHAR(50),
    CONSTRAINT "fk_発注明細_発注"
        FOREIGN KEY ("発注番号") REFERENCES "発注データ"("発注番号"),
    UNIQUE ("発注番号", "発注行番号")
);

-- 諸口品目情報（マスタに登録されていない臨時品目）
CREATE TABLE "諸口品目情報" (
    "ID" SERIAL PRIMARY KEY,
    "発注番号" VARCHAR(20) NOT NULL,
    "発注行番号" INTEGER NOT NULL,
    "品目コード" VARCHAR(20) NOT NULL,
    "品名" VARCHAR(100) NOT NULL,
    "規格" VARCHAR(100),
    "図番メーカー" VARCHAR(100),
    "版数" VARCHAR(20),
    "作成日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "更新日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    UNIQUE ("発注番号", "発注行番号", "品目コード")
);

-- インデックス
CREATE INDEX "idx_発注データ_取引先コード" ON "発注データ"("取引先コード");
CREATE INDEX "idx_発注データ_発注日" ON "発注データ"("発注日");
CREATE INDEX "idx_発注明細_発注番号" ON "発注明細データ"("発注番号");
CREATE INDEX "idx_発注明細_品目コード" ON "発注明細データ"("品目コード");
CREATE INDEX "idx_単価マスタ_品目取引先" ON "単価マスタ"("品目コード", "取引先コード");
