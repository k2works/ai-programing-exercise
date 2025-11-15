-- 与信残高テーブルの作成
CREATE TABLE IF NOT EXISTS "与信残高" (
  "取引先コード" VARCHAR(8) PRIMARY KEY,
  "受注残高" INTEGER NOT NULL DEFAULT 0,
  "売掛残高" INTEGER NOT NULL DEFAULT 0,
  "買掛残高" INTEGER NOT NULL DEFAULT 0,
  "作成日時" TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  "作成者名" VARCHAR(12),
  "更新日時" TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  "更新者名" VARCHAR(12),
  CONSTRAINT fk_credit_balance_company FOREIGN KEY ("取引先コード")
    REFERENCES "取引先マスタ"("取引先コード")
);

-- インデックスの作成
CREATE INDEX idx_credit_balance_company ON "与信残高"("取引先コード");

-- コメントの追加
COMMENT ON TABLE "与信残高" IS '取引先ごとの与信状況を管理するテーブル';
COMMENT ON COLUMN "与信残高"."取引先コード" IS '与信管理対象の取引先コード';
COMMENT ON COLUMN "与信残高"."受注残高" IS '未出荷の受注金額の合計';
COMMENT ON COLUMN "与信残高"."売掛残高" IS '未入金の売上金額の合計(売掛金)';
COMMENT ON COLUMN "与信残高"."買掛残高" IS '未払いの仕入金額の合計(買掛金)';
