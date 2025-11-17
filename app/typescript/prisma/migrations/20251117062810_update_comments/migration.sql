-- Prisma Database Comments Generator v1.4.0

-- 勘定科目マスタ comments
COMMENT ON TABLE "勘定科目マスタ" IS '勘定科目マスタ（財務会計システムの基本となる勘定科目情報）';
COMMENT ON COLUMN "勘定科目マスタ"."勘定科目コード" IS '勘定科目コード（主キー）';
COMMENT ON COLUMN "勘定科目マスタ"."勘定科目名" IS '勘定科目名';
COMMENT ON COLUMN "勘定科目マスタ"."勘定科目カナ" IS '勘定科目カナ';
COMMENT ON COLUMN "勘定科目マスタ"."勘定科目種別" IS '勘定科目種別';
COMMENT ON COLUMN "勘定科目マスタ"."合計科目" IS '合計科目';
COMMENT ON COLUMN "勘定科目マスタ"."BSPL区分" IS 'BSPL区分';
COMMENT ON COLUMN "勘定科目マスタ"."取引要素区分" IS '取引要素区分';
COMMENT ON COLUMN "勘定科目マスタ"."費用区分" IS '費用区分';
COMMENT ON COLUMN "勘定科目マスタ"."表示順序" IS '表示順序';
COMMENT ON COLUMN "勘定科目マスタ"."集計対象" IS '集計対象';
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

-- 勘定科目構成マスタ comments
COMMENT ON TABLE "勘定科目構成マスタ" IS '勘定科目構成マスタ（チルダ連結方式による階層構造管理）';
COMMENT ON COLUMN "勘定科目構成マスタ"."勘定科目コード" IS '勘定科目コード（主キー、外部キー）';
COMMENT ON COLUMN "勘定科目構成マスタ"."勘定科目パス" IS '勘定科目パス（チルダ連結、例：11^11000^11190^11110）';
COMMENT ON COLUMN "勘定科目構成マスタ"."作成日時" IS '作成日時';
COMMENT ON COLUMN "勘定科目構成マスタ"."更新日時" IS '更新日時';
