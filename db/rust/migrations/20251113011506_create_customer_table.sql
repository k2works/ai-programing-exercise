-- 顧客マスタテーブルの作成

CREATE TABLE "顧客マスタ" (
    "顧客コード" VARCHAR(8) NOT NULL,
    "顧客枝番" INTEGER NOT NULL,
    "顧客区分" INTEGER DEFAULT 0,
    "請求先コード" VARCHAR(8) NOT NULL,
    "請求先枝番" INTEGER,
    "回収先コード" VARCHAR(8) NOT NULL,
    "回収先枝番" INTEGER,
    "顧客名" VARCHAR(40) NOT NULL,
    "顧客名カナ" VARCHAR(40),
    "自社担当者コード" VARCHAR(10) NOT NULL,
    "顧客締日１" INTEGER NOT NULL,
    "顧客支払月１" INTEGER DEFAULT 1,
    "顧客支払日１" INTEGER,
    "作成日時" TIMESTAMP(6) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "作成者名" VARCHAR(12),
    "更新日時" TIMESTAMP(6) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "更新者名" VARCHAR(12),

    CONSTRAINT pk_customer PRIMARY KEY ("顧客コード", "顧客枝番"),

    -- 取引先マスタへの外部キー制約
    CONSTRAINT fk_customer_company
        FOREIGN KEY ("顧客コード")
        REFERENCES "取引先マスタ" ("取引先コード")
        ON DELETE RESTRICT
        ON UPDATE CASCADE,

    -- 社員マスタへの外部キー制約
    CONSTRAINT fk_customer_employee
        FOREIGN KEY ("自社担当者コード")
        REFERENCES "社員マスタ" ("社員コード")
        ON DELETE RESTRICT
        ON UPDATE CASCADE
);

CREATE INDEX idx_customer_company_code ON "顧客マスタ" ("顧客コード");
CREATE INDEX idx_customer_emp_code ON "顧客マスタ" ("自社担当者コード");

COMMENT ON TABLE "顧客マスタ" IS '取引先の「顧客」としての役割情報を管理するテーブル';
COMMENT ON COLUMN "顧客マスタ"."顧客コード" IS '取引先コード（外部キー）';
COMMENT ON COLUMN "顧客マスタ"."顧客枝番" IS '同じ取引先の複数の部門などを区別するための枝番';
COMMENT ON COLUMN "顧客マスタ"."顧客区分" IS '顧客の区分（法人、個人など）';
COMMENT ON COLUMN "顧客マスタ"."請求先コード" IS '請求書を送付する先のコード';
COMMENT ON COLUMN "顧客マスタ"."回収先コード" IS '代金を回収する先のコード';
COMMENT ON COLUMN "顧客マスタ"."自社担当者コード" IS 'この顧客を担当する社員（外部キー）';
COMMENT ON COLUMN "顧客マスタ"."顧客締日１" IS '請求締め日（例: 月末=31）';
COMMENT ON COLUMN "顧客マスタ"."顧客支払月１" IS '支払月（締め後何ヶ月後）';
COMMENT ON COLUMN "顧客マスタ"."顧客支払日１" IS '支払日';
