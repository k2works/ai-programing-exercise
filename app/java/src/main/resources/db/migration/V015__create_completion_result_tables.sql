-- 完成実績データ
CREATE TABLE "完成実績データ" (
    "ID" SERIAL PRIMARY KEY,
    "完成実績番号" VARCHAR(20) UNIQUE NOT NULL,
    "作業指示番号" VARCHAR(20) NOT NULL,
    "品目コード" VARCHAR(20) NOT NULL,
    "完成日" DATE NOT NULL,
    "完成数量" DECIMAL(15, 2) NOT NULL,
    "良品数" DECIMAL(15, 2) NOT NULL,
    "不良品数" DECIMAL(15, 2) NOT NULL,
    "備考" TEXT,
    "作成日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "作成者" VARCHAR(50),
    "更新日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "更新者" VARCHAR(50),
    CONSTRAINT "fk_完成実績_作業指示"
        FOREIGN KEY ("作業指示番号") REFERENCES "作業指示データ"("作業指示番号")
);

-- 完成検査結果データ
CREATE TABLE "完成検査結果データ" (
    "ID" SERIAL PRIMARY KEY,
    "完成実績番号" VARCHAR(20) NOT NULL,
    "欠点コード" VARCHAR(20) NOT NULL,
    "数量" DECIMAL(15, 2) NOT NULL,
    "作成日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "更新日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT "fk_完成検査結果_完成実績"
        FOREIGN KEY ("完成実績番号") REFERENCES "完成実績データ"("完成実績番号"),
    CONSTRAINT "fk_完成検査結果_欠点"
        FOREIGN KEY ("欠点コード") REFERENCES "欠点マスタ"("欠点コード"),
    UNIQUE ("完成実績番号", "欠点コード")
);

-- インデックス
CREATE INDEX "idx_完成実績_作業指示番号" ON "完成実績データ"("作業指示番号");
CREATE INDEX "idx_完成実績_品目コード" ON "完成実績データ"("品目コード");
CREATE INDEX "idx_完成実績_完成日" ON "完成実績データ"("完成日");
