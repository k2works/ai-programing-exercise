-- 仕入データテーブルの作成

CREATE TABLE "仕入データ" (
    "仕入番号" VARCHAR(10) NOT NULL,
    "仕入日" TIMESTAMP(6),
    "仕入先コード" VARCHAR(8) NOT NULL,
    "仕入先枝番" INTEGER DEFAULT 0,
    "仕入担当者コード" VARCHAR(10) NOT NULL,
    "開始日" TIMESTAMP(6) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "発注番号" VARCHAR(10),
    "部門コード" VARCHAR(6) NOT NULL,
    "仕入金額合計" INTEGER DEFAULT 0,
    "消費税合計" INTEGER NOT NULL DEFAULT 0,
    "備考" VARCHAR(1000),
    "作成日時" TIMESTAMP(6) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "作成者名" VARCHAR(12),
    "更新日時" TIMESTAMP(6) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "更新者名" VARCHAR(12),

    CONSTRAINT pk_purchase PRIMARY KEY ("仕入番号"),

    -- 仕入先マスタへの外部キー制約
    CONSTRAINT fk_purchase_supplier
        FOREIGN KEY ("仕入先コード", "仕入先枝番")
        REFERENCES "仕入先マスタ" ("仕入先コード", "仕入先枝番")
        ON DELETE RESTRICT
        ON UPDATE CASCADE,

    -- 社員マスタへの外部キー制約
    CONSTRAINT fk_purchase_employee
        FOREIGN KEY ("仕入担当者コード")
        REFERENCES "社員マスタ" ("社員コード")
        ON DELETE RESTRICT
        ON UPDATE CASCADE,

    -- 部門マスタへの外部キー制約
    CONSTRAINT fk_purchase_department
        FOREIGN KEY ("部門コード", "開始日")
        REFERENCES "部門マスタ" ("部門コード", "開始日")
        ON DELETE RESTRICT
        ON UPDATE CASCADE
);

CREATE INDEX idx_purchase_sup_code ON "仕入データ" ("仕入先コード", "仕入先枝番");
CREATE INDEX idx_purchase_emp_code ON "仕入データ" ("仕入担当者コード");
CREATE INDEX idx_purchase_date ON "仕入データ" ("仕入日");
CREATE INDEX idx_purchase_po_no ON "仕入データ" ("発注番号");

COMMENT ON TABLE "仕入データ" IS '商品の仕入・入荷を記録するトランザクションテーブル';
COMMENT ON COLUMN "仕入データ"."仕入番号" IS '仕入を一意に識別する番号';
COMMENT ON COLUMN "仕入データ"."仕入日" IS '仕入を計上した日付';
COMMENT ON COLUMN "仕入データ"."仕入先コード" IS '仕入先（外部キー）';
COMMENT ON COLUMN "仕入データ"."仕入担当者コード" IS '仕入を担当した社員（外部キー）';
COMMENT ON COLUMN "仕入データ"."発注番号" IS '元となった発注番号（任意）';
COMMENT ON COLUMN "仕入データ"."仕入金額合計" IS '仕入の合計金額';
