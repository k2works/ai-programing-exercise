-- トリガー関数：仕訳の借方貸方合計をチェック
CREATE OR REPLACE FUNCTION validate_journal_balance()
RETURNS TRIGGER AS $$
DECLARE
    v_debit_total DECIMAL(15, 2);
    v_credit_total DECIMAL(15, 2);
    v_journal_no VARCHAR(20);
BEGIN
    -- INSERT/UPDATE の場合は NEW.仕訳伝票番号、DELETE の場合は OLD.仕訳伝票番号
    IF TG_OP = 'DELETE' THEN
        v_journal_no := OLD."仕訳伝票番号";
    ELSE
        v_journal_no := NEW."仕訳伝票番号";
    END IF;

    -- 仕訳の借方・貸方合計を計算
    SELECT
        COALESCE(SUM(CASE WHEN "仕訳行貸借区分" = 'D' THEN "仕訳金額" ELSE 0 END), 0),
        COALESCE(SUM(CASE WHEN "仕訳行貸借区分" = 'C' THEN "仕訳金額" ELSE 0 END), 0)
    INTO v_debit_total, v_credit_total
    FROM "仕訳貸借明細"
    WHERE "仕訳伝票番号" = v_journal_no;

    -- 借方合計 ≠ 貸方合計ならエラー
    -- ただし、まだ明細が1件も登録されていない場合（両方0）は許可する
    IF v_debit_total <> v_credit_total AND (v_debit_total <> 0 OR v_credit_total <> 0) THEN
        RAISE EXCEPTION '借方合計(%)と貸方合計(%)が一致しません。伝票番号: %',
            v_debit_total, v_credit_total, v_journal_no;
    END IF;

    -- DELETE の場合は OLD を、それ以外は NEW を返す
    IF TG_OP = 'DELETE' THEN
        RETURN OLD;
    ELSE
        RETURN NEW;
    END IF;
END;
$$ LANGUAGE plpgsql;

-- INSERT/UPDATE/DELETE 時にトリガー発火（トランザクション終了時に実行）
CREATE CONSTRAINT TRIGGER trigger_validate_journal_balance
AFTER INSERT OR UPDATE OR DELETE ON "仕訳貸借明細"
DEFERRABLE INITIALLY DEFERRED
FOR EACH ROW
EXECUTE FUNCTION validate_journal_balance();

-- トリガーのコメント
COMMENT ON FUNCTION validate_journal_balance() IS '複式簿記の原理（借方合計 = 貸方合計）を保証するトリガー関数';
