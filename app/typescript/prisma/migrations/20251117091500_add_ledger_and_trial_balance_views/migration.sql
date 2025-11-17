-- 総勘定元帳ビュー（日次残高から生成）
CREATE OR REPLACE VIEW "総勘定元帳" AS
SELECT
  d."起票日" as entry_date,
  a."勘定科目コード" as account_code,
  a."勘定科目名" as account_name,
  a."BSPL区分" as bspl_type,
  d."補助科目コード" as sub_account_code,
  d."部門コード" as department_code,
  d."プロジェクトコード" as project_code,
  d."借方金額" as debit_amount,
  d."貸方金額" as credit_amount,
  SUM(d."借方金額" - d."貸方金額") OVER (
    PARTITION BY d."勘定科目コード", d."補助科目コード", d."部門コード", d."プロジェクトコード"
    ORDER BY d."起票日"
  ) as balance
FROM "日次勘定科目残高" d
INNER JOIN "勘定科目マスタ" a ON d."勘定科目コード" = a."勘定科目コード"
ORDER BY d."勘定科目コード", d."起票日";

COMMENT ON VIEW "総勘定元帳" IS '総勘定元帳（日次残高から生成される勘定科目ごとの取引履歴）';

-- 試算表ビュー（日次残高から生成）
CREATE OR REPLACE VIEW "試算表" AS
SELECT
  a."勘定科目コード" as account_code,
  a."勘定科目名" as account_name,
  a."BSPL区分" as bspl_type,
  a."取引要素区分" as transaction_type,
  COALESCE(SUM(d."借方金額"), 0) as debit_total,
  COALESCE(SUM(d."貸方金額"), 0) as credit_total,
  COALESCE(SUM(d."借方金額"), 0) - COALESCE(SUM(d."貸方金額"), 0) as balance
FROM "勘定科目マスタ" a
LEFT JOIN "日次勘定科目残高" d ON a."勘定科目コード" = d."勘定科目コード"
GROUP BY a."勘定科目コード", a."勘定科目名", a."BSPL区分", a."取引要素区分"
ORDER BY a."勘定科目コード";

COMMENT ON VIEW "試算表" IS '試算表（全勘定科目の残高一覧）';
