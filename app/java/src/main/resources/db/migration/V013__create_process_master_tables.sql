-- 工程マスタ
CREATE TABLE "工程マスタ" (
    "工程コード" VARCHAR(20) PRIMARY KEY,
    "工程名" VARCHAR(100) NOT NULL,
    "作成日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "作成者" VARCHAR(50),
    "更新日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "更新者" VARCHAR(50)
);

-- 工程表
CREATE TABLE "工程表" (
    "ID" SERIAL PRIMARY KEY,
    "品目コード" VARCHAR(20) NOT NULL,
    "工順" INTEGER NOT NULL,
    "工程コード" VARCHAR(20) NOT NULL,
    "作成日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "更新日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT "fk_工程表_工程"
        FOREIGN KEY ("工程コード") REFERENCES "工程マスタ"("工程コード"),
    UNIQUE ("品目コード", "工順")
);

-- インデックス
CREATE INDEX "idx_工程表_品目コード" ON "工程表"("品目コード");
CREATE INDEX "idx_工程表_工程コード" ON "工程表"("工程コード");
