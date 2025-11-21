-- 勘定科目マスタに実務項目を追加

-- BSPL区分を追加
ALTER TABLE "勘定科目マスタ"
    ADD COLUMN "BSPL区分" CHAR(1);

-- 取引要素区分を追加
ALTER TABLE "勘定科目マスタ"
    ADD COLUMN "取引要素区分" CHAR(1);

-- 費用区分を追加
ALTER TABLE "勘定科目マスタ"
    ADD COLUMN "費用区分" CHAR(1);

-- 合計科目フラグを追加
ALTER TABLE "勘定科目マスタ"
    ADD COLUMN "合計科目" BOOLEAN DEFAULT false NOT NULL;

-- 表示順序を追加
ALTER TABLE "勘定科目マスタ"
    ADD COLUMN "表示順序" INTEGER;

-- 集計対象フラグを追加
ALTER TABLE "勘定科目マスタ"
    ADD COLUMN "集計対象" BOOLEAN DEFAULT true NOT NULL;

-- 勘定科目カナを追加
ALTER TABLE "勘定科目マスタ"
    ADD COLUMN "勘定科目カナ" VARCHAR(40);

-- コメント追加
COMMENT ON COLUMN "勘定科目マスタ"."BSPL区分" IS
    'BSPL区分（B:貸借対照表, P:損益計算書）';

COMMENT ON COLUMN "勘定科目マスタ"."取引要素区分" IS
    '取引要素区分（1:資産, 2:負債, 3:純資産, 4:収益, 5:費用）';

COMMENT ON COLUMN "勘定科目マスタ"."費用区分" IS
    '費用区分（1:売上原価, 2:販売費及び一般管理費, 3:営業外費用）';

COMMENT ON COLUMN "勘定科目マスタ"."合計科目" IS
    '合計科目（true: 集計科目, false: 明細科目）';

COMMENT ON COLUMN "勘定科目マスタ"."表示順序" IS
    '表示順序（財務諸表での表示順）';

COMMENT ON COLUMN "勘定科目マスタ"."集計対象" IS
    '集計対象（true: 集計対象, false: 集計対象外）';

COMMENT ON COLUMN "勘定科目マスタ"."勘定科目カナ" IS
    '勘定科目カナ（検索用）';
