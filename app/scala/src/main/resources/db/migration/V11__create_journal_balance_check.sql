-- ==========================================
-- 複式簿記の原理をデータベースで保証するための関数
-- ==========================================

-- 1. 仕訳の貸借平衡をチェックする関数
CREATE OR REPLACE FUNCTION check_journal_balance()
RETURNS TABLE(
  journal_no VARCHAR,
  debit_total NUMERIC,
  credit_total NUMERIC,
  diff NUMERIC
) AS $$
BEGIN
  RETURN QUERY
  SELECT
    jdc."仕訳伝票番号"::VARCHAR,
    SUM(CASE WHEN jdc."仕訳行貸借区分" = 'D' THEN jdc."仕訳金額" ELSE 0 END) AS debit_total,
    SUM(CASE WHEN jdc."仕訳行貸借区分" = 'C' THEN jdc."仕訳金額" ELSE 0 END) AS credit_total,
    SUM(CASE WHEN jdc."仕訳行貸借区分" = 'D' THEN jdc."仕訳金額" ELSE -jdc."仕訳金額" END) AS diff
  FROM "仕訳貸借明細" jdc
  GROUP BY jdc."仕訳伝票番号"
  HAVING SUM(CASE WHEN jdc."仕訳行貸借区分" = 'D' THEN jdc."仕訳金額" ELSE -jdc."仕訳金額" END) <> 0;
END;
$$ LANGUAGE plpgsql;

COMMENT ON FUNCTION check_journal_balance() IS '貸借不平衡の仕訳を検出する関数';

-- 2. 特定の仕訳の貸借平衡をチェックする関数
CREATE OR REPLACE FUNCTION validate_journal_balance(p_journal_no VARCHAR)
RETURNS TABLE(
  is_balanced BOOLEAN,
  debit_total NUMERIC,
  credit_total NUMERIC,
  diff NUMERIC
) AS $$
BEGIN
  RETURN QUERY
  SELECT
    (SUM(CASE WHEN jdc."仕訳行貸借区分" = 'D' THEN jdc."仕訳金額" ELSE -jdc."仕訳金額" END) = 0) AS is_balanced,
    SUM(CASE WHEN jdc."仕訳行貸借区分" = 'D' THEN jdc."仕訳金額" ELSE 0 END) AS debit_total,
    SUM(CASE WHEN jdc."仕訳行貸借区分" = 'C' THEN jdc."仕訳金額" ELSE 0 END) AS credit_total,
    SUM(CASE WHEN jdc."仕訳行貸借区分" = 'D' THEN jdc."仕訳金額" ELSE -jdc."仕訳金額" END) AS diff
  FROM "仕訳貸借明細" jdc
  WHERE jdc."仕訳伝票番号" = p_journal_no;
END;
$$ LANGUAGE plpgsql;

COMMENT ON FUNCTION validate_journal_balance(VARCHAR) IS '特定の仕訳の貸借平衡を検証する関数';

-- 3. 3層構造の仕訳の貸借平衡をチェックするビュー
CREATE OR REPLACE VIEW "v_仕訳貸借チェック" AS
SELECT
  jdc."仕訳伝票番号",
  SUM(CASE WHEN jdc."仕訳行貸借区分" = 'D' THEN jdc."仕訳金額" ELSE 0 END) AS debit_total,
  SUM(CASE WHEN jdc."仕訳行貸借区分" = 'C' THEN jdc."仕訳金額" ELSE 0 END) AS credit_total,
  SUM(CASE WHEN jdc."仕訳行貸借区分" = 'D' THEN jdc."仕訳金額" ELSE -jdc."仕訳金額" END) AS diff,
  CASE
    WHEN SUM(CASE WHEN jdc."仕訳行貸借区分" = 'D' THEN jdc."仕訳金額" ELSE -jdc."仕訳金額" END) = 0 THEN TRUE
    ELSE FALSE
  END AS is_balanced
FROM "仕訳貸借明細" jdc
GROUP BY jdc."仕訳伝票番号";

COMMENT ON VIEW "v_仕訳貸借チェック" IS '仕訳の貸借平衡チェックビュー';

-- 4. 不平衡仕訳のみを表示するビュー
CREATE OR REPLACE VIEW "v_不平衡仕訳" AS
SELECT *
FROM "v_仕訳貸借チェック"
WHERE is_balanced = FALSE;

COMMENT ON VIEW "v_不平衡仕訳" IS '貸借不平衡の仕訳のみを表示するビュー';
