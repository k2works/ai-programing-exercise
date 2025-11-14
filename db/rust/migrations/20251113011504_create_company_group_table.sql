-- 取引先グループマスタテーブルの作成

CREATE TABLE "取引先グループマスタ" (
    "取引先グループコード" VARCHAR(4) NOT NULL,
    "取引先グループ名" VARCHAR(40),
    "作成日時" TIMESTAMP(6) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "作成者名" VARCHAR(12),
    "更新日時" TIMESTAMP(6) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "更新者名" VARCHAR(12),

    CONSTRAINT pk_company_group_mst PRIMARY KEY ("取引先グループコード")
);

COMMENT ON TABLE "取引先グループマスタ" IS '取引先をグループ化するためのマスタテーブル';
COMMENT ON COLUMN "取引先グループマスタ"."取引先グループコード" IS '取引先グループを一意に識別するコード';
COMMENT ON COLUMN "取引先グループマスタ"."取引先グループ名" IS 'グループの名称';
