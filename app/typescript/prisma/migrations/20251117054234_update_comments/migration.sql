-- Prisma Database Comments Generator v1.4.0

-- 勘定科目マスタ comments
COMMENT ON TABLE "勘定科目マスタ" IS '勘定科目マスタ（財務会計システムの基本となる勘定科目情報）';
COMMENT ON COLUMN "勘定科目マスタ"."勘定科目ID" IS '勘定科目ID（主キー）';
COMMENT ON COLUMN "勘定科目マスタ"."勘定科目コード" IS '勘定科目コード（例：1000, 2000）';
COMMENT ON COLUMN "勘定科目マスタ"."勘定科目名" IS '勘定科目名（例：現金、売掛金）';
COMMENT ON COLUMN "勘定科目マスタ"."勘定科目種別" IS '勘定科目種別（資産、負債、純資産、収益、費用）';
COMMENT ON COLUMN "勘定科目マスタ"."残高" IS '残高';
COMMENT ON COLUMN "勘定科目マスタ"."課税取引コード" IS '課税取引コード';
COMMENT ON COLUMN "勘定科目マスタ"."作成日時" IS '作成日時';
COMMENT ON COLUMN "勘定科目マスタ"."更新日時" IS '更新日時';

-- 課税取引マスタ comments
COMMENT ON TABLE "課税取引マスタ" IS '課税取引マスタ（消費税計算のための課税区分情報）';
COMMENT ON COLUMN "課税取引マスタ"."課税取引コード" IS '課税取引コード（主キー）';
COMMENT ON COLUMN "課税取引マスタ"."課税取引名" IS '課税取引名';
COMMENT ON COLUMN "課税取引マスタ"."税率" IS '税率（%）';
COMMENT ON COLUMN "課税取引マスタ"."課税区分" IS '課税区分（課税、非課税、免税、不課税）';
COMMENT ON COLUMN "課税取引マスタ"."説明" IS '説明';
COMMENT ON COLUMN "課税取引マスタ"."適用開始日" IS '適用開始日';
COMMENT ON COLUMN "課税取引マスタ"."適用終了日" IS '適用終了日';
COMMENT ON COLUMN "課税取引マスタ"."作成日時" IS '作成日時';
COMMENT ON COLUMN "課税取引マスタ"."更新日時" IS '更新日時';
