-- BSPL区分を追加
ALTER TABLE "勘定科目マスタ"
ADD COLUMN "BSPL区分" CHAR(1);

COMMENT ON COLUMN "勘定科目マスタ"."BSPL区分" IS
    'BSPL区分（B:貸借対照表, P:損益計算書）';
