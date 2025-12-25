-- V021__create_cost_tables.sql
-- 製造原価管理テーブル

-- 標準原価マスタ
CREATE TABLE "標準原価マスタ" (
    "ID" SERIAL PRIMARY KEY,
    "品目コード" VARCHAR(20) NOT NULL,
    "適用開始日" DATE NOT NULL,
    "適用終了日" DATE,
    "標準材料費" DECIMAL(15, 2) NOT NULL,
    "標準労務費" DECIMAL(15, 2) NOT NULL,
    "標準経費" DECIMAL(15, 2) NOT NULL,
    "標準製造原価" DECIMAL(15, 2) NOT NULL,
    "作成日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "更新日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT "UK_標準原価_品目_適用開始日" UNIQUE ("品目コード", "適用開始日")
);

COMMENT ON TABLE "標準原価マスタ" IS '標準原価マスタ';
COMMENT ON COLUMN "標準原価マスタ"."標準材料費" IS '標準材料費（単位あたり）';
COMMENT ON COLUMN "標準原価マスタ"."標準労務費" IS '標準労務費（単位あたり）';
COMMENT ON COLUMN "標準原価マスタ"."標準経費" IS '標準経費（単位あたり）';
COMMENT ON COLUMN "標準原価マスタ"."標準製造原価" IS '標準製造原価（単位あたり）';

-- 実際原価データ
CREATE TABLE "実際原価データ" (
    "ID" SERIAL PRIMARY KEY,
    "作業指示番号" VARCHAR(20) UNIQUE NOT NULL,
    "品目コード" VARCHAR(20) NOT NULL,
    "完成数量" DECIMAL(15, 2) NOT NULL,
    "実際材料費" DECIMAL(15, 2) NOT NULL,
    "実際労務費" DECIMAL(15, 2) NOT NULL,
    "実際経費" DECIMAL(15, 2) NOT NULL,
    "実際製造原価" DECIMAL(15, 2) NOT NULL,
    "単位原価" DECIMAL(15, 4) NOT NULL,
    "作成日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "更新日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL
);

COMMENT ON TABLE "実際原価データ" IS '実際原価データ';
COMMENT ON COLUMN "実際原価データ"."実際材料費" IS '実際材料費（合計）';
COMMENT ON COLUMN "実際原価データ"."実際労務費" IS '実際労務費（合計）';
COMMENT ON COLUMN "実際原価データ"."実際経費" IS '実際経費（合計）';
COMMENT ON COLUMN "実際原価データ"."単位原価" IS '完成品1単位あたりの原価';

-- 原価差異データ
CREATE TABLE "原価差異データ" (
    "ID" SERIAL PRIMARY KEY,
    "作業指示番号" VARCHAR(20) NOT NULL,
    "品目コード" VARCHAR(20) NOT NULL,
    "材料費差異" DECIMAL(15, 2) NOT NULL,
    "労務費差異" DECIMAL(15, 2) NOT NULL,
    "経費差異" DECIMAL(15, 2) NOT NULL,
    "総差異" DECIMAL(15, 2) NOT NULL,
    "作成日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT "UK_原価差異_作業指示" UNIQUE ("作業指示番号")
);

COMMENT ON TABLE "原価差異データ" IS '原価差異データ';
COMMENT ON COLUMN "原価差異データ"."材料費差異" IS '材料費差異（実際 - 標準）';
COMMENT ON COLUMN "原価差異データ"."労務費差異" IS '労務費差異（実際 - 標準）';
COMMENT ON COLUMN "原価差異データ"."経費差異" IS '経費差異（実際 - 標準）';
COMMENT ON COLUMN "原価差異データ"."総差異" IS '原価総差異';

CREATE INDEX "IDX_標準原価_品目" ON "標準原価マスタ" ("品目コード");
CREATE INDEX "IDX_標準原価_適用期間" ON "標準原価マスタ" ("適用開始日", "適用終了日");
CREATE INDEX "IDX_実際原価_作業指示" ON "実際原価データ" ("作業指示番号");
CREATE INDEX "IDX_実際原価_品目" ON "実際原価データ" ("品目コード");
CREATE INDEX "IDX_原価差異_品目" ON "原価差異データ" ("品目コード");
