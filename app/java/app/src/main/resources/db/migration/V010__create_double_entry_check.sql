-- ==========================================
-- 複式簿記の原理をデータベースで保証する
-- ==========================================

-- 仕訳ごとの借方・貸方の合計を算出するビュー
CREATE OR REPLACE VIEW "仕訳残高チェック" AS
SELECT
  "仕訳伝票番号",
  SUM(CASE WHEN "仕訳行貸借区分" = 'D' THEN "仕訳金額" ELSE 0 END) AS "借方合計",
  SUM(CASE WHEN "仕訳行貸借区分" = 'C' THEN "仕訳金額" ELSE 0 END) AS "貸方合計",
  SUM(CASE WHEN "仕訳行貸借区分" = 'D' THEN "仕訳金額" ELSE 0 END) -
  SUM(CASE WHEN "仕訳行貸借区分" = 'C' THEN "仕訳金額" ELSE 0 END) AS "差額"
FROM "仕訳貸借明細"
GROUP BY "仕訳伝票番号";

-- ビューのコメント
COMMENT ON VIEW "仕訳残高チェック" IS '仕訳ごとの借方・貸方の合計を算出するビュー';

-- 差額が0でない仕訳を検出する関数
CREATE OR REPLACE FUNCTION "複式簿記チェック"()
RETURNS TABLE("不整合伝票番号" VARCHAR(20), "差額" NUMERIC) AS $$
BEGIN
  RETURN QUERY
  SELECT "仕訳伝票番号", ("借方合計" - "貸方合計") as "差額"
  FROM "仕訳残高チェック"
  WHERE "借方合計" != "貸方合計";
END;
$$ LANGUAGE plpgsql;

-- 関数のコメント
COMMENT ON FUNCTION "複式簿記チェック"() IS '複式簿記の原理（借方合計 = 貸方合計）に違反する仕訳を検出する関数';
