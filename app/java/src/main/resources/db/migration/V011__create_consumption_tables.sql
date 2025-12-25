-- 消費データ
-- FK constraint for 取引先マスタ is omitted due to composite primary key
CREATE TABLE "消費データ" (
    "ID" SERIAL PRIMARY KEY,
    "消費番号" VARCHAR(20) UNIQUE NOT NULL,
    "入荷番号" VARCHAR(20) NOT NULL,
    "消費日" DATE NOT NULL,
    "取引先コード" VARCHAR(20) NOT NULL,
    "備考" TEXT,
    "作成日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "作成者" VARCHAR(50),
    "更新日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "更新者" VARCHAR(50),
    CONSTRAINT "fk_消費データ_入荷"
        FOREIGN KEY ("入荷番号") REFERENCES "入荷受入データ"("入荷番号")
);

-- 消費明細データ
-- FK constraint for 品目マスタ is omitted due to composite primary key
CREATE TABLE "消費明細データ" (
    "ID" SERIAL PRIMARY KEY,
    "消費番号" VARCHAR(20) NOT NULL,
    "消費行番号" INTEGER NOT NULL,
    "品目コード" VARCHAR(20) NOT NULL,
    "消費数量" DECIMAL(15, 2) NOT NULL,
    "備考" TEXT,
    "作成日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "更新日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT "fk_消費明細_消費"
        FOREIGN KEY ("消費番号") REFERENCES "消費データ"("消費番号"),
    UNIQUE ("消費番号", "消費行番号")
);

-- インデックス
CREATE INDEX "idx_消費データ_入荷番号" ON "消費データ"("入荷番号");
CREATE INDEX "idx_消費データ_取引先コード" ON "消費データ"("取引先コード");
CREATE INDEX "idx_消費データ_消費日" ON "消費データ"("消費日");
CREATE INDEX "idx_消費明細_品目コード" ON "消費明細データ"("品目コード");
