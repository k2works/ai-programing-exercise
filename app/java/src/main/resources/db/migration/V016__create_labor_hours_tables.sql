-- V016__create_labor_hours_tables.sql

-- 部門マスタ
CREATE TABLE "部門マスタ" (
    "部門コード" VARCHAR(20) PRIMARY KEY,
    "部門名" VARCHAR(100) NOT NULL,
    "作成日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "作成者" VARCHAR(50),
    "更新日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "更新者" VARCHAR(50)
);

-- 担当者マスタ
CREATE TABLE "担当者マスタ" (
    "担当者コード" VARCHAR(20) PRIMARY KEY,
    "担当者名" VARCHAR(100) NOT NULL,
    "部門コード" VARCHAR(20),
    "作成日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "作成者" VARCHAR(50),
    "更新日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "更新者" VARCHAR(50),
    CONSTRAINT "fk_担当者_部門"
        FOREIGN KEY ("部門コード") REFERENCES "部門マスタ"("部門コード")
);

-- 工数実績データ
CREATE TABLE "工数実績データ" (
    "ID" SERIAL PRIMARY KEY,
    "工数実績番号" VARCHAR(20) UNIQUE NOT NULL,
    "作業指示番号" VARCHAR(20) NOT NULL,
    "品目コード" VARCHAR(20) NOT NULL,
    "工順" INTEGER NOT NULL,
    "工程コード" VARCHAR(20) NOT NULL,
    "部門コード" VARCHAR(20) NOT NULL,
    "担当者コード" VARCHAR(20) NOT NULL,
    "作業日" DATE NOT NULL,
    "工数" DECIMAL(10, 2) NOT NULL,
    "備考" TEXT,
    "作成日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "作成者" VARCHAR(50),
    "更新日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "更新者" VARCHAR(50),
    CONSTRAINT "fk_工数実績_作業指示"
        FOREIGN KEY ("作業指示番号") REFERENCES "作業指示データ"("作業指示番号"),
    CONSTRAINT "fk_工数実績_工程"
        FOREIGN KEY ("工程コード") REFERENCES "工程マスタ"("工程コード"),
    CONSTRAINT "fk_工数実績_部門"
        FOREIGN KEY ("部門コード") REFERENCES "部門マスタ"("部門コード"),
    CONSTRAINT "fk_工数実績_担当者"
        FOREIGN KEY ("担当者コード") REFERENCES "担当者マスタ"("担当者コード")
);

-- インデックス
CREATE INDEX "idx_担当者_部門コード" ON "担当者マスタ"("部門コード");
CREATE INDEX "idx_工数実績_作業指示番号" ON "工数実績データ"("作業指示番号");
CREATE INDEX "idx_工数実績_品目コード" ON "工数実績データ"("品目コード");
CREATE INDEX "idx_工数実績_工程コード" ON "工数実績データ"("工程コード");
CREATE INDEX "idx_工数実績_担当者コード" ON "工数実績データ"("担当者コード");
CREATE INDEX "idx_工数実績_作業日" ON "工数実績データ"("作業日");
