-- BSPL区分のCHECK制約
ALTER TABLE "勘定科目マスタ"
    ADD CONSTRAINT check_bspl_distinction
    CHECK ("BSPL区分" IN ('B', 'P') OR "BSPL区分" IS NULL);

-- 貸借区分のCHECK制約
ALTER TABLE "勘定科目マスタ"
    ADD CONSTRAINT check_debit_credit_distinction
    CHECK ("貸借区分" IN ('D', 'C') OR "貸借区分" IS NULL);

-- 集計区分のCHECK制約
ALTER TABLE "勘定科目マスタ"
    ADD CONSTRAINT check_aggregation_distinction
    CHECK ("集計区分" IN ('H', 'S', 'D') OR "集計区分" IS NULL);

-- BSPL区分と勘定科目種別の整合性チェック
ALTER TABLE "勘定科目マスタ"
    ADD CONSTRAINT check_bspl_consistency
    CHECK (
        ("BSPL区分" = 'B' AND "勘定科目種別"::text IN ('資産', '負債', '純資産'))
        OR
        ("BSPL区分" = 'P' AND "勘定科目種別"::text IN ('収益', '費用'))
        OR
        ("BSPL区分" IS NULL)
    );

-- 貸借区分と勘定科目種別の整合性チェック
ALTER TABLE "勘定科目マスタ"
    ADD CONSTRAINT check_debit_credit_consistency
    CHECK (
        ("貸借区分" = 'D' AND "勘定科目種別"::text IN ('資産', '費用'))
        OR
        ("貸借区分" = 'C' AND "勘定科目種別"::text IN ('負債', '純資産', '収益'))
        OR
        ("貸借区分" IS NULL)
    );

COMMENT ON CONSTRAINT check_bspl_distinction ON "勘定科目マスタ" IS
    'BSPL区分は B（貸借対照表）または P（損益計算書）のみ許可';

COMMENT ON CONSTRAINT check_bspl_consistency ON "勘定科目マスタ" IS
    'BSPL区分と勘定科目種別の整合性を保証';

COMMENT ON CONSTRAINT check_debit_credit_consistency ON "勘定科目マスタ" IS
    '貸借区分と勘定科目種別の整合性を保証';
