-- 取引先マスタテーブルの作成

CREATE TABLE "取引先マスタ" (
    "取引先コード" VARCHAR(8) NOT NULL,
    "取引先名" VARCHAR(40) NOT NULL,
    "取引先名カナ" VARCHAR(40),
    "仕入先区分" INTEGER DEFAULT 0,
    "郵便番号" CHAR(8),
    "都道府県" VARCHAR(4),
    "住所１" VARCHAR(40),
    "住所２" VARCHAR(40),
    "取引禁止フラグ" INTEGER DEFAULT 0,
    "雑区分" INTEGER DEFAULT 0,
    "取引先グループコード" VARCHAR(4) NOT NULL,
    "与信限度額" INTEGER DEFAULT 0,
    "与信一時増加枠" INTEGER DEFAULT 0,
    "作成日時" TIMESTAMP(6) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "作成者名" VARCHAR(12),
    "更新日時" TIMESTAMP(6) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "更新者名" VARCHAR(12),

    CONSTRAINT pk_companys_mst PRIMARY KEY ("取引先コード"),

    -- 取引先グループマスタへの外部キー制約
    CONSTRAINT fk_company_group
        FOREIGN KEY ("取引先グループコード")
        REFERENCES "取引先グループマスタ" ("取引先グループコード")
        ON DELETE RESTRICT
        ON UPDATE CASCADE
);

CREATE INDEX idx_company_group_code ON "取引先マスタ" ("取引先グループコード");
CREATE INDEX idx_company_name ON "取引先マスタ" ("取引先名");
CREATE INDEX idx_company_kana ON "取引先マスタ" ("取引先名カナ");

COMMENT ON TABLE "取引先マスタ" IS 'すべての取引先の基本情報を管理するマスタテーブル';
COMMENT ON COLUMN "取引先マスタ"."取引先コード" IS '取引先を一意に識別するコード';
COMMENT ON COLUMN "取引先マスタ"."取引先名" IS '取引先の名称';
COMMENT ON COLUMN "取引先マスタ"."仕入先区分" IS '仕入先としての区分';
COMMENT ON COLUMN "取引先マスタ"."取引禁止フラグ" IS '取引を禁止するフラグ（0:可、1:不可）';
COMMENT ON COLUMN "取引先マスタ"."取引先グループコード" IS '所属する取引先グループ（外部キー）';
COMMENT ON COLUMN "取引先マスタ"."与信限度額" IS '与信の上限金額';
COMMENT ON COLUMN "取引先マスタ"."与信一時増加枠" IS '一時的な与信増加枠';
