-- 受注データテーブルの作成

CREATE TABLE "受注データ" (
    "受注番号" VARCHAR(10) NOT NULL,
    "受注日" TIMESTAMP(6),
    "納品予定日" TIMESTAMP(6),
    "顧客コード" VARCHAR(8) NOT NULL,
    "顧客枝番" INTEGER DEFAULT 0,
    "敬称区分" INTEGER DEFAULT 0,
    "郵便番号" CHAR(8),
    "住所１" VARCHAR(40),
    "住所２" VARCHAR(40),
    "社員コード" VARCHAR(10) NOT NULL,
    "部門コード" VARCHAR(6) NOT NULL,
    "明細行数" INTEGER DEFAULT 0,
    "受注金額" INTEGER DEFAULT 0,
    "消費税額" INTEGER NOT NULL DEFAULT 0,
    "伝票備考" VARCHAR(1000),
    "作成日時" TIMESTAMP(6) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "作成者名" VARCHAR(12),
    "更新日時" TIMESTAMP(6) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "更新者名" VARCHAR(12),

    CONSTRAINT pk_order PRIMARY KEY ("受注番号"),

    -- 顧客マスタへの外部キー制約
    CONSTRAINT fk_order_customer
        FOREIGN KEY ("顧客コード", "顧客枝番")
        REFERENCES "顧客マスタ" ("顧客コード", "顧客枝番")
        ON DELETE RESTRICT
        ON UPDATE CASCADE,

    -- 社員マスタへの外部キー制約
    CONSTRAINT fk_order_employee
        FOREIGN KEY ("社員コード")
        REFERENCES "社員マスタ" ("社員コード")
        ON DELETE RESTRICT
        ON UPDATE CASCADE
);

CREATE INDEX idx_order_cust_code ON "受注データ" ("顧客コード", "顧客枝番");
CREATE INDEX idx_order_emp_code ON "受注データ" ("社員コード");
CREATE INDEX idx_order_date ON "受注データ" ("受注日");

COMMENT ON TABLE "受注データ" IS '顧客からの注文を記録するトランザクションテーブル';
COMMENT ON COLUMN "受注データ"."受注番号" IS '受注を一意に識別する番号';
COMMENT ON COLUMN "受注データ"."受注日" IS '注文を受けた日付';
COMMENT ON COLUMN "受注データ"."納品予定日" IS '納品予定の日付';
COMMENT ON COLUMN "受注データ"."顧客コード" IS '注文した顧客（外部キー）';
COMMENT ON COLUMN "受注データ"."社員コード" IS '受注を担当した社員（外部キー）';
COMMENT ON COLUMN "受注データ"."受注金額" IS '受注の合計金額';
