-- 勘定科目カナを追加
ALTER TABLE "勘定科目マスタ"
    ADD COLUMN "勘定科目カナ" VARCHAR(100);

COMMENT ON COLUMN "勘定科目マスタ"."勘定科目カナ" IS
    '勘定科目名のカナ表記';

-- 合計科目フラグを追加
ALTER TABLE "勘定科目マスタ"
    ADD COLUMN "合計科目" BOOLEAN NOT NULL DEFAULT FALSE;

COMMENT ON COLUMN "勘定科目マスタ"."合計科目" IS
    '合計科目かどうか（TRUE: 合計科目、FALSE: 明細科目）';

-- 取引要素区分を追加
ALTER TABLE "勘定科目マスタ"
    ADD COLUMN "取引要素区分" VARCHAR(10);

COMMENT ON COLUMN "勘定科目マスタ"."取引要素区分" IS
    '取引要素区分（資金/損益/etc）';

-- 費用区分を追加
ALTER TABLE "勘定科目マスタ"
    ADD COLUMN "費用区分" VARCHAR(10);

COMMENT ON COLUMN "勘定科目マスタ"."費用区分" IS
    '費用区分（製造原価/販管費/営業外/etc）';

-- 表示順序を追加
ALTER TABLE "勘定科目マスタ"
    ADD COLUMN "表示順序" INTEGER NOT NULL DEFAULT 0;

COMMENT ON COLUMN "勘定科目マスタ"."表示順序" IS
    '勘定科目の表示順序（昇順）';

-- 集計対象フラグを追加
ALTER TABLE "勘定科目マスタ"
    ADD COLUMN "集計対象" BOOLEAN NOT NULL DEFAULT TRUE;

COMMENT ON COLUMN "勘定科目マスタ"."集計対象" IS
    '集計対象かどうか（TRUE: 集計対象、FALSE: 集計対象外）';

-- 表示順序のインデックスを追加
CREATE INDEX idx_account_display_order
    ON "勘定科目マスタ" ("表示順序");

COMMENT ON INDEX idx_account_display_order IS
    '表示順序での並び替えを高速化';
