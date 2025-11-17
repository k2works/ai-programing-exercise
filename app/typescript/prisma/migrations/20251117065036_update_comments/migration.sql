-- Prisma Database Comments Generator v1.4.0

-- 仕訳エントリ comments
COMMENT ON TABLE "仕訳エントリ" IS '仕訳エントリ（複式簿記の仕訳データ）';
COMMENT ON COLUMN "仕訳エントリ"."伝票番号" IS '伝票番号（主キー）';
COMMENT ON COLUMN "仕訳エントリ"."仕訳日" IS '仕訳日';
COMMENT ON COLUMN "仕訳エントリ"."摘要" IS '摘要';
COMMENT ON COLUMN "仕訳エントリ"."合計金額" IS '合計金額';
COMMENT ON COLUMN "仕訳エントリ"."参照番号" IS '参照番号';
COMMENT ON COLUMN "仕訳エントリ"."作成者" IS '作成者';
COMMENT ON COLUMN "仕訳エントリ"."作成日時" IS '作成日時';
COMMENT ON COLUMN "仕訳エントリ"."更新者" IS '更新者';
COMMENT ON COLUMN "仕訳エントリ"."更新日時" IS '更新日時';

-- 仕訳明細 comments
COMMENT ON TABLE "仕訳明細" IS '仕訳明細（仕訳エントリの明細行データ）';
COMMENT ON COLUMN "仕訳明細"."伝票番号" IS '伝票番号（複合主キー1）';
COMMENT ON COLUMN "仕訳明細"."行番号" IS '行番号（複合主キー2）';
COMMENT ON COLUMN "仕訳明細"."勘定科目コード" IS '勘定科目コード';
COMMENT ON COLUMN "仕訳明細"."借方金額" IS '借方金額';
COMMENT ON COLUMN "仕訳明細"."貸方金額" IS '貸方金額';
COMMENT ON COLUMN "仕訳明細"."摘要" IS '摘要';
COMMENT ON COLUMN "仕訳明細"."消費税額" IS '消費税額';
COMMENT ON COLUMN "仕訳明細"."消費税率" IS '消費税率';
