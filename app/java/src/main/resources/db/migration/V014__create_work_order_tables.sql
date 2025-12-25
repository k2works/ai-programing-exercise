-- 作業指示ステータス
CREATE TYPE 作業指示ステータス AS ENUM ('未着手', '作業中', '完了', '中断');

-- 作業指示データ
CREATE TABLE "作業指示データ" (
    "ID" SERIAL PRIMARY KEY,
    "作業指示番号" VARCHAR(20) UNIQUE NOT NULL,
    "オーダ番号" VARCHAR(20) NOT NULL,
    "作業指示日" DATE NOT NULL,
    "品目コード" VARCHAR(20) NOT NULL,
    "作業指示数" DECIMAL(15, 2) NOT NULL,
    "場所コード" VARCHAR(20) NOT NULL,
    "開始予定日" DATE NOT NULL,
    "完成予定日" DATE NOT NULL,
    "実績開始日" DATE,
    "実績完了日" DATE,
    "完成済数" DECIMAL(15, 2) DEFAULT 0 NOT NULL,
    "総良品数" DECIMAL(15, 2) DEFAULT 0 NOT NULL,
    "総不良品数" DECIMAL(15, 2) DEFAULT 0 NOT NULL,
    "ステータス" 作業指示ステータス DEFAULT '未着手' NOT NULL,
    "完了フラグ" BOOLEAN DEFAULT false NOT NULL,
    "備考" TEXT,
    "作成日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "作成者" VARCHAR(50),
    "更新日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "更新者" VARCHAR(50),
    CONSTRAINT "fk_作業指示_オーダ"
        FOREIGN KEY ("オーダ番号") REFERENCES "オーダ情報"("オーダNO"),
    CONSTRAINT "fk_作業指示_場所"
        FOREIGN KEY ("場所コード") REFERENCES "場所マスタ"("場所コード")
);

-- 作業指示明細データ
CREATE TABLE "作業指示明細データ" (
    "ID" SERIAL PRIMARY KEY,
    "作業指示番号" VARCHAR(20) NOT NULL,
    "工順" INTEGER NOT NULL,
    "工程コード" VARCHAR(20) NOT NULL,
    "作成日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "更新日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT "fk_作業指示明細_作業指示"
        FOREIGN KEY ("作業指示番号") REFERENCES "作業指示データ"("作業指示番号"),
    CONSTRAINT "fk_作業指示明細_工程"
        FOREIGN KEY ("工程コード") REFERENCES "工程マスタ"("工程コード"),
    UNIQUE ("作業指示番号", "工順")
);

-- インデックス
CREATE INDEX "idx_作業指示_オーダ番号" ON "作業指示データ"("オーダ番号");
CREATE INDEX "idx_作業指示_品目コード" ON "作業指示データ"("品目コード");
CREATE INDEX "idx_作業指示_ステータス" ON "作業指示データ"("ステータス");
CREATE INDEX "idx_作業指示明細_工程コード" ON "作業指示明細データ"("工程コード");
