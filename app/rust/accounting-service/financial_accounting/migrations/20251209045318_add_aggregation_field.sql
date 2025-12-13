-- 集計区分を追加
ALTER TABLE "勘定科目マスタ"
ADD COLUMN "集計区分" CHAR(1);

COMMENT ON COLUMN "勘定科目マスタ"."集計区分" IS
    '集計区分（H:見出科目, S:集計科目, D:明細科目）';
