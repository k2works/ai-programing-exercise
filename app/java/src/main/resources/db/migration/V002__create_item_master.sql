-- 単位マスタ
CREATE TABLE "単位マスタ" (
    "単位コード" VARCHAR(10) PRIMARY KEY,
    "単位記号" VARCHAR(10) NOT NULL,
    "単位名" VARCHAR(50) NOT NULL,
    "作成日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    "更新日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- 品目マスタ
CREATE TABLE "品目マスタ" (
    "ID" SERIAL PRIMARY KEY,
    "品目コード" VARCHAR(20) NOT NULL,
    "適用開始日" DATE NOT NULL,
    "適用停止日" DATE,
    "品名" VARCHAR(100) NOT NULL,
    "品目区分" 品目区分 NOT NULL,
    "単位コード" VARCHAR(10) REFERENCES "単位マスタ"("単位コード"),
    "リードタイム" INTEGER DEFAULT 0,
    "安全リードタイム" INTEGER DEFAULT 0,
    "安全在庫数" DECIMAL(15, 2) DEFAULT 0,
    "歩留率" DECIMAL(5, 2) DEFAULT 100,
    "最小ロット数" DECIMAL(15, 2) DEFAULT 1,
    "刻みロット数" DECIMAL(15, 2) DEFAULT 1,
    "最大ロット数" DECIMAL(15, 2),
    "有効期間" INTEGER,
    "作成日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    "更新日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    UNIQUE("品目コード", "適用開始日")
);

-- インデックス
CREATE INDEX idx_品目マスタ_品目コード ON "品目マスタ"("品目コード");
CREATE INDEX idx_品目マスタ_品目区分 ON "品目マスタ"("品目区分");
