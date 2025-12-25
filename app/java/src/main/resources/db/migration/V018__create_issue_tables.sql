-- V018__create_issue_tables.sql
-- 払出業務テーブル

-- 払出指示データ
CREATE TABLE "払出指示データ" (
    "ID" SERIAL PRIMARY KEY,
    "払出指示番号" VARCHAR(20) UNIQUE NOT NULL,
    "オーダ番号" VARCHAR(20) NOT NULL,
    "払出指示日" DATE NOT NULL,
    "場所コード" VARCHAR(20) NOT NULL,
    "備考" VARCHAR(500),
    "作成日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "更新日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT "FK_払出指示_オーダ" FOREIGN KEY ("オーダ番号")
        REFERENCES "オーダ情報"("オーダNO"),
    CONSTRAINT "FK_払出指示_場所" FOREIGN KEY ("場所コード")
        REFERENCES "場所マスタ"("場所コード")
);

COMMENT ON TABLE "払出指示データ" IS '払出指示データ';
COMMENT ON COLUMN "払出指示データ"."払出指示番号" IS '払出指示番号';
COMMENT ON COLUMN "払出指示データ"."オーダ番号" IS 'オーダ番号';
COMMENT ON COLUMN "払出指示データ"."払出指示日" IS '払出指示日';

-- 払出指示明細データ
CREATE TABLE "払出指示明細データ" (
    "ID" SERIAL PRIMARY KEY,
    "払出指示番号" VARCHAR(20) NOT NULL,
    "払出行番号" INT NOT NULL,
    "品目コード" VARCHAR(20) NOT NULL,
    "工順" INT NOT NULL,
    "払出数量" DECIMAL(15, 2) NOT NULL,
    "作成日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "更新日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT "UK_払出指示明細" UNIQUE ("払出指示番号", "払出行番号"),
    CONSTRAINT "FK_払出指示明細_払出指示" FOREIGN KEY ("払出指示番号")
        REFERENCES "払出指示データ"("払出指示番号")
);

COMMENT ON TABLE "払出指示明細データ" IS '払出指示明細データ';

-- 払出データ
CREATE TABLE "払出データ" (
    "ID" SERIAL PRIMARY KEY,
    "払出番号" VARCHAR(20) UNIQUE NOT NULL,
    "作業指示番号" VARCHAR(20) NOT NULL,
    "工順" INT NOT NULL,
    "場所コード" VARCHAR(20) NOT NULL,
    "払出日" DATE NOT NULL,
    "払出担当者コード" VARCHAR(20) NOT NULL,
    "作成日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "更新日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT "FK_払出_場所" FOREIGN KEY ("場所コード")
        REFERENCES "場所マスタ"("場所コード")
);

COMMENT ON TABLE "払出データ" IS '払出データ';
COMMENT ON COLUMN "払出データ"."払出番号" IS '払出番号';
COMMENT ON COLUMN "払出データ"."作業指示番号" IS '作業指示番号';

-- 払出明細データ
CREATE TABLE "払出明細データ" (
    "ID" SERIAL PRIMARY KEY,
    "払出番号" VARCHAR(20) NOT NULL,
    "払出行番号" INT NOT NULL,
    "品目コード" VARCHAR(20) NOT NULL,
    "払出数" DECIMAL(15, 2) NOT NULL,
    "作成日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "更新日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT "UK_払出明細" UNIQUE ("払出番号", "払出行番号"),
    CONSTRAINT "FK_払出明細_払出" FOREIGN KEY ("払出番号")
        REFERENCES "払出データ"("払出番号")
);

COMMENT ON TABLE "払出明細データ" IS '払出明細データ';

CREATE INDEX "IDX_払出指示_オーダ" ON "払出指示データ" ("オーダ番号");
CREATE INDEX "IDX_払出指示_場所" ON "払出指示データ" ("場所コード");
CREATE INDEX "IDX_払出指示明細_品目" ON "払出指示明細データ" ("品目コード");
CREATE INDEX "IDX_払出_作業指示" ON "払出データ" ("作業指示番号");
CREATE INDEX "IDX_払出明細_品目" ON "払出明細データ" ("品目コード");
