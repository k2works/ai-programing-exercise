-- 勘定科目マスタに制約を追加

-- BSPL区分のCHECK制約（'B' または 'P' のみ許可）
ALTER TABLE "勘定科目マスタ"
    ADD CONSTRAINT chk_bspl_distinction
    CHECK ("BSPL区分" IN ('B', 'P') OR "BSPL区分" IS NULL);

-- 取引要素区分のCHECK制約（1-5 のみ許可）
ALTER TABLE "勘定科目マスタ"
    ADD CONSTRAINT chk_transaction_element
    CHECK ("取引要素区分" IN ('1', '2', '3', '4', '5') OR "取引要素区分" IS NULL);

-- 費用区分のCHECK制約（1-3 のみ許可）
ALTER TABLE "勘定科目マスタ"
    ADD CONSTRAINT chk_expense_distinction
    CHECK ("費用区分" IN ('1', '2', '3') OR "費用区分" IS NULL);

-- 残高のCHECK制約（妥当な範囲）
ALTER TABLE "勘定科目マスタ"
    ADD CONSTRAINT chk_balance_range
    CHECK ("残高" >= -999999999999.99 AND "残高" <= 999999999999.99);

-- 表示順序のCHECK制約（正の整数）
ALTER TABLE "勘定科目マスタ"
    ADD CONSTRAINT chk_display_order
    CHECK ("表示順序" > 0 OR "表示順序" IS NULL);
