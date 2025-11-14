-- 売上データテーブルの作成

CREATE TABLE "売上データ" (
    "売上番号" VARCHAR(10) NOT NULL,
    "売上日" TIMESTAMP(6),
    "顧客コード" VARCHAR(8) NOT NULL,
    "顧客枝番" INTEGER DEFAULT 0,
    "社員コード" VARCHAR(10) NOT NULL,
    "部門コード" VARCHAR(6) NOT NULL,
    "開始日" TIMESTAMP(6) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "倉庫コード" VARCHAR(3) NOT NULL,
    "受注番号" VARCHAR(10),
    "明細行数" INTEGER DEFAULT 0,
    "売上金額" INTEGER DEFAULT 0,
    "消費税額" INTEGER NOT NULL DEFAULT 0,
    "伝票備考" VARCHAR(1000),
    "作成日時" TIMESTAMP(6) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "作成者名" VARCHAR(12),
    "更新日時" TIMESTAMP(6) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "更新者名" VARCHAR(12),

    CONSTRAINT pk_sales PRIMARY KEY ("売上番号"),

    -- 顧客マスタへの外部キー制約
    CONSTRAINT fk_sales_customer
        FOREIGN KEY ("顧客コード", "顧客枝番")
        REFERENCES "顧客マスタ" ("顧客コード", "顧客枝番")
        ON DELETE RESTRICT
        ON UPDATE CASCADE,

    -- 社員マスタへの外部キー制約
    CONSTRAINT fk_sales_employee
        FOREIGN KEY ("社員コード")
        REFERENCES "社員マスタ" ("社員コード")
        ON DELETE RESTRICT
        ON UPDATE CASCADE,

    -- 部門マスタへの外部キー制約
    CONSTRAINT fk_sales_department
        FOREIGN KEY ("部門コード", "開始日")
        REFERENCES "部門マスタ" ("部門コード", "開始日")
        ON DELETE RESTRICT
        ON UPDATE CASCADE
);

CREATE INDEX idx_sales_cust_code ON "売上データ" ("顧客コード", "顧客枝番");
CREATE INDEX idx_sales_emp_code ON "売上データ" ("社員コード");
CREATE INDEX idx_sales_date ON "売上データ" ("売上日");
CREATE INDEX idx_sales_order_no ON "売上データ" ("受注番号");

COMMENT ON TABLE "売上データ" IS '商品の出荷・売上を記録するトランザクションテーブル';
COMMENT ON COLUMN "売上データ"."売上番号" IS '売上を一意に識別する番号';
COMMENT ON COLUMN "売上データ"."売上日" IS '売上を計上した日付';
COMMENT ON COLUMN "売上データ"."顧客コード" IS '売上先の顧客（外部キー）';
COMMENT ON COLUMN "売上データ"."社員コード" IS '売上を担当した社員（外部キー）';
COMMENT ON COLUMN "売上データ"."受注番号" IS '元となった受注番号（任意）';
COMMENT ON COLUMN "売上データ"."売上金額" IS '売上の合計金額';
