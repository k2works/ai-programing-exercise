-- V017__create_inventory_tables.sql

-- 倉庫マスタ
CREATE TABLE "倉庫マスタ" (
    "倉庫コード" VARCHAR(20) PRIMARY KEY,
    "倉庫区分" VARCHAR(20) NOT NULL,
    "倉庫名" VARCHAR(100) NOT NULL,
    "部門コード" VARCHAR(20),
    "作成日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "更新日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT "fk_倉庫_部門"
        FOREIGN KEY ("部門コード") REFERENCES "部門マスタ"("部門コード")
);

-- 在庫情報
CREATE TABLE "在庫情報" (
    "ID" SERIAL PRIMARY KEY,
    "場所コード" VARCHAR(20) NOT NULL,
    "品目コード" VARCHAR(20) NOT NULL,
    "在庫数量" DECIMAL(15, 2) NOT NULL DEFAULT 0,
    "合格数" DECIMAL(15, 2) NOT NULL DEFAULT 0,
    "不良数" DECIMAL(15, 2) NOT NULL DEFAULT 0,
    "未検査数" DECIMAL(15, 2) NOT NULL DEFAULT 0,
    "作成日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "更新日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT "uk_在庫_場所_品目" UNIQUE ("場所コード", "品目コード"),
    CONSTRAINT "fk_在庫_場所"
        FOREIGN KEY ("場所コード") REFERENCES "場所マスタ"("場所コード")
);

-- インデックス
CREATE INDEX "idx_倉庫_部門コード" ON "倉庫マスタ"("部門コード");
CREATE INDEX "idx_在庫_場所コード" ON "在庫情報"("場所コード");
CREATE INDEX "idx_在庫_品目コード" ON "在庫情報"("品目コード");
