-- ==========================================
-- 総勘定元帳ビュー
-- ==========================================

CREATE OR REPLACE VIEW "総勘定元帳" AS
SELECT
    d."起票日" as entry_date,
    a."勘定科目コード" as account_code,
    a."勘定科目名" as account_name,
    a."勘定科目種別"::text as account_type,
    d."補助科目コード" as sub_account_code,
    d."部門コード" as department_code,
    d."プロジェクトコード" as project_code,
    d."借方金額" as debit_amount,
    d."貸方金額" as credit_amount,
    -- ウィンドウ関数で累積残高を計算
    SUM(d."借方金額" - d."貸方金額") OVER (
        PARTITION BY d."勘定科目コード", d."補助科目コード", d."部門コード", d."プロジェクトコード"
        ORDER BY d."起票日"
        ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW
    ) as balance
FROM "日次勘定科目残高" d
INNER JOIN "勘定科目マスタ" a ON d."勘定科目コード" = a."勘定科目コード"
WHERE d."決算仕訳フラグ" = 0  -- 通常仕訳のみ
ORDER BY d."勘定科目コード", d."起票日";

COMMENT ON VIEW "総勘定元帳" IS '総勘定元帳（日次残高から生成される勘定科目ごとの取引履歴）';

-- ==========================================
-- 試算表ビュー
-- ==========================================

CREATE OR REPLACE VIEW "試算表" AS
SELECT
    a."勘定科目コード" as account_code,
    a."勘定科目名" as account_name,
    a."勘定科目種別"::text as account_type,
    -- 勘定科目種別から貸借区分を判定（資産・費用=借方、負債・純資産・収益=貸方）
    CASE
        WHEN a."勘定科目種別"::text IN ('資産', '費用') THEN '借'
        ELSE '貸'
    END as debit_credit_type,
    COALESCE(SUM(d."借方金額"), 0) as debit_total,
    COALESCE(SUM(d."貸方金額"), 0) as credit_total,
    -- 貸借区分に応じて残高を計算
    CASE
        WHEN a."勘定科目種別"::text IN ('資産', '費用') THEN
            COALESCE(SUM(d."借方金額"), 0) - COALESCE(SUM(d."貸方金額"), 0)
        ELSE
            COALESCE(SUM(d."貸方金額"), 0) - COALESCE(SUM(d."借方金額"), 0)
    END as balance
FROM "勘定科目マスタ" a
LEFT JOIN "日次勘定科目残高" d
    ON a."勘定科目コード" = d."勘定科目コード"
    AND d."決算仕訳フラグ" = 0  -- 通常仕訳のみ
GROUP BY a."勘定科目コード", a."勘定科目名", a."勘定科目種別"
ORDER BY a."勘定科目コード";

COMMENT ON VIEW "試算表" IS '試算表（全勘定科目の残高一覧）';

-- ==========================================
-- 日次残高更新用関数（UPSERT）
-- ==========================================

CREATE OR REPLACE FUNCTION upsert_daily_balance(
    p_entry_date DATE,
    p_account_code VARCHAR(10),
    p_sub_account_code VARCHAR(10),
    p_department_code VARCHAR(5),
    p_project_code VARCHAR(10),
    p_settlement_flag INTEGER,
    p_debit_amount NUMERIC(15,2),
    p_credit_amount NUMERIC(15,2)
) RETURNS VOID AS $$
BEGIN
    INSERT INTO "日次勘定科目残高" (
        "起票日", "勘定科目コード", "補助科目コード", "部門コード",
        "プロジェクトコード", "決算仕訳フラグ", "借方金額", "貸方金額"
    ) VALUES (
        p_entry_date, p_account_code,
        COALESCE(p_sub_account_code, ''),
        COALESCE(p_department_code, ''),
        COALESCE(p_project_code, ''),
        COALESCE(p_settlement_flag, 0),
        p_debit_amount, p_credit_amount
    )
    ON CONFLICT (
        "起票日", "勘定科目コード", "補助科目コード", "部門コード",
        "プロジェクトコード", "決算仕訳フラグ"
    )
    DO UPDATE SET
        "借方金額" = "日次勘定科目残高"."借方金額" + EXCLUDED."借方金額",
        "貸方金額" = "日次勘定科目残高"."貸方金額" + EXCLUDED."貸方金額",
        "更新日時" = CURRENT_TIMESTAMP;
END;
$$ LANGUAGE plpgsql;

COMMENT ON FUNCTION upsert_daily_balance IS '日次残高を更新（同じキーが存在する場合は加算）';

-- ==========================================
-- 月次締め処理関数
-- ==========================================

CREATE OR REPLACE FUNCTION close_monthly_balance(
    p_fiscal_year INTEGER,
    p_month INTEGER
) RETURNS INTEGER AS $$
DECLARE
    v_affected_rows INTEGER;
BEGIN
    INSERT INTO "月次勘定科目残高" (
        "決算期", "月度", "勘定科目コード", "補助科目コード",
        "部門コード", "プロジェクトコード", "決算仕訳フラグ",
        "月初残高", "借方金額", "貸方金額", "月末残高"
    )
    SELECT
        p_fiscal_year,
        p_month,
        "勘定科目コード",
        "補助科目コード",
        "部門コード",
        "プロジェクトコード",
        "決算仕訳フラグ",
        0 as "月初残高",  -- 前月末残高から取得（別途処理）
        SUM("借方金額") as "借方金額",
        SUM("貸方金額") as "貸方金額",
        0 + SUM("借方金額") - SUM("貸方金額") as "月末残高"
    FROM "日次勘定科目残高"
    WHERE EXTRACT(YEAR FROM "起票日") = p_fiscal_year
      AND EXTRACT(MONTH FROM "起票日") = p_month
    GROUP BY
        "勘定科目コード",
        "補助科目コード",
        "部門コード",
        "プロジェクトコード",
        "決算仕訳フラグ"
    ON CONFLICT (
        "決算期", "月度", "勘定科目コード", "補助科目コード",
        "部門コード", "プロジェクトコード", "決算仕訳フラグ"
    )
    DO UPDATE SET
        "借方金額" = EXCLUDED."借方金額",
        "貸方金額" = EXCLUDED."貸方金額",
        "月末残高" = EXCLUDED."月末残高",
        "更新日時" = CURRENT_TIMESTAMP;

    GET DIAGNOSTICS v_affected_rows = ROW_COUNT;
    RETURN v_affected_rows;
END;
$$ LANGUAGE plpgsql;

COMMENT ON FUNCTION close_monthly_balance IS '月次締め処理（日次残高を集計して月次残高を更新）';
