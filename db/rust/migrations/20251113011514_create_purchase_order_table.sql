-- 発注データテーブルの作成

CREATE TABLE "発注データ" (
    "発注番号" VARCHAR(10) NOT NULL,
    "発注日" TIMESTAMP(6),
    "仕入先コード" VARCHAR(8) NOT NULL,
    "仕入先枝番" INTEGER DEFAULT 0,
    "発注担当者コード" VARCHAR(10) NOT NULL,
    "指定納期" TIMESTAMP(6),
    "倉庫コード" VARCHAR(3) NOT NULL,
    "発注金額合計" INTEGER DEFAULT 0,
    "消費税合計" INTEGER NOT NULL DEFAULT 0,
    "備考" VARCHAR(1000),
    "作成日時" TIMESTAMP(6) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "作成者名" VARCHAR(12),
    "更新日時" TIMESTAMP(6) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "更新者名" VARCHAR(12),

    CONSTRAINT pk_purchase_order PRIMARY KEY ("発注番号"),

    -- 仕入先マスタへの外部キー制約
    CONSTRAINT fk_po_supplier
        FOREIGN KEY ("仕入先コード", "仕入先枝番")
        REFERENCES "仕入先マスタ" ("仕入先コード", "仕入先枝番")
        ON DELETE RESTRICT
        ON UPDATE CASCADE,

    -- 社員マスタへの外部キー制約
    CONSTRAINT fk_po_employee
        FOREIGN KEY ("発注担当者コード")
        REFERENCES "社員マスタ" ("社員コード")
        ON DELETE RESTRICT
        ON UPDATE CASCADE,

    -- 倉庫マスタへの外部キー制約
    CONSTRAINT fk_po_warehouse
        FOREIGN KEY ("倉庫コード")
        REFERENCES "倉庫マスタ" ("倉庫コード")
        ON DELETE RESTRICT
        ON UPDATE CASCADE
);

CREATE INDEX idx_po_sup_code ON "発注データ" ("仕入先コード", "仕入先枝番");
CREATE INDEX idx_po_emp_code ON "発注データ" ("発注担当者コード");
CREATE INDEX idx_po_date ON "発注データ" ("発注日");

COMMENT ON TABLE "発注データ" IS '仕入先への発注を記録するトランザクションテーブル';
COMMENT ON COLUMN "発注データ"."発注番号" IS '発注を一意に識別する番号';
COMMENT ON COLUMN "発注データ"."発注日" IS '発注を行った日付';
COMMENT ON COLUMN "発注データ"."仕入先コード" IS '発注先の仕入先（外部キー）';
COMMENT ON COLUMN "発注データ"."発注担当者コード" IS '発注を担当した社員（外部キー）';
COMMENT ON COLUMN "発注データ"."指定納期" IS '納品を希望する日付';
COMMENT ON COLUMN "発注データ"."倉庫コード" IS '納品先の倉庫（外部キー）';
COMMENT ON COLUMN "発注データ"."発注金額合計" IS '発注の合計金額';
