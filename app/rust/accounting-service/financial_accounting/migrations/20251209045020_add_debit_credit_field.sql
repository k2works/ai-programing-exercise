-- 貸借区分を追加
ALTER TABLE "勘定科目マスタ"
ADD COLUMN "貸借区分" CHAR(1);

COMMENT ON COLUMN "勘定科目マスタ"."貸借区分" IS
    '貸借区分（D:借方, C:貸方）';
