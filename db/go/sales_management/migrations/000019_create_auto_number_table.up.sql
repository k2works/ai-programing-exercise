-- 自動採番テーブルの作成
CREATE TABLE IF NOT EXISTS "自動採番" (
  "伝票種別" VARCHAR(2) NOT NULL,
  "年月" TIMESTAMP NOT NULL,
  "最終伝票番号" INTEGER NOT NULL DEFAULT 0,
  PRIMARY KEY ("伝票種別", "年月")
);

-- インデックスの作成
CREATE INDEX idx_auto_number_year_month ON "自動採番"("年月");

-- コメントの追加
COMMENT ON TABLE "自動採番" IS '伝票番号を自動採番するためのマスタテーブル';
COMMENT ON COLUMN "自動採番"."伝票種別" IS '伝票の種類を識別するコード(OR:受注、SL:売上、IV:請求など)';
COMMENT ON COLUMN "自動採番"."年月" IS '採番の対象となる年月';
COMMENT ON COLUMN "自動採番"."最終伝票番号" IS '最後に採番された番号';
