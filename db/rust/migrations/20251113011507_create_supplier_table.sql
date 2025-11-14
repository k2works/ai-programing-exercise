-- 仕入先マスタテーブルの作成

CREATE TABLE "仕入先マスタ" (
    "仕入先コード" VARCHAR(8) NOT NULL,
    "仕入先枝番" INTEGER NOT NULL,
    "仕入先名" VARCHAR(40) NOT NULL,
    "仕入先名カナ" VARCHAR(40),
    "仕入先締日" INTEGER NOT NULL,
    "仕入先支払月" INTEGER DEFAULT 1,
    "仕入先支払日" INTEGER,
    "仕入先支払方法" INTEGER DEFAULT 1,
    "作成日時" TIMESTAMP(6) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "作成者名" VARCHAR(12),
    "更新日時" TIMESTAMP(6) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "更新者名" VARCHAR(12),

    CONSTRAINT pk_supplier PRIMARY KEY ("仕入先コード", "仕入先枝番"),

    -- 取引先マスタへの外部キー制約
    CONSTRAINT fk_supplier_company
        FOREIGN KEY ("仕入先コード")
        REFERENCES "取引先マスタ" ("取引先コード")
        ON DELETE RESTRICT
        ON UPDATE CASCADE
);

CREATE INDEX idx_supplier_company_code ON "仕入先マスタ" ("仕入先コード");

COMMENT ON TABLE "仕入先マスタ" IS '取引先の「仕入先」としての役割情報を管理するテーブル';
COMMENT ON COLUMN "仕入先マスタ"."仕入先コード" IS '取引先コード（外部キー）';
COMMENT ON COLUMN "仕入先マスタ"."仕入先枝番" IS '同じ取引先の複数の拠点などを区別するための枝番';
COMMENT ON COLUMN "仕入先マスタ"."仕入先締日" IS '仕入の締め日';
COMMENT ON COLUMN "仕入先マスタ"."仕入先支払月" IS '支払月（締め後何ヶ月後）';
COMMENT ON COLUMN "仕入先マスタ"."仕入先支払日" IS '支払日';
COMMENT ON COLUMN "仕入先マスタ"."仕入先支払方法" IS '支払方法（1:現金、2:振込など）';
