-- 発注データ明細
-- 目的: 発注の商品別明細情報を管理
-- 特徴: 発注番号+行番号の複合主キー
CREATE TABLE 発注データ明細 (
    発注番号 VARCHAR(10) NOT NULL,
    発注行番号 INTEGER NOT NULL,
    商品コード VARCHAR(20) NOT NULL,
    商品名 VARCHAR(200) NOT NULL,
    発注単価 INTEGER DEFAULT 0,
    発注数量 INTEGER DEFAULT 1,
    入荷予定日 TIMESTAMP,
    入荷済数量 INTEGER DEFAULT 0,
    完了フラグ INTEGER DEFAULT 0,
    作成日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    作成者名 VARCHAR(100),
    更新日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    更新者名 VARCHAR(100),

    PRIMARY KEY (発注番号, 発注行番号),

    CONSTRAINT fk_po_detail_po FOREIGN KEY (発注番号)
        REFERENCES 発注データ(発注番号) ON DELETE CASCADE,
    CONSTRAINT fk_po_detail_product FOREIGN KEY (商品コード)
        REFERENCES 商品マスタ(商品コード)
);

COMMENT ON TABLE 発注データ明細 IS '発注の商品別明細情報';
COMMENT ON COLUMN 発注データ明細.発注番号 IS '親の発注データへの外部キー';
COMMENT ON COLUMN 発注データ明細.発注行番号 IS '発注内での明細行番号';
COMMENT ON COLUMN 発注データ明細.商品コード IS '発注した商品';
COMMENT ON COLUMN 発注データ明細.商品名 IS '発注時の商品名（履歴保持）';
COMMENT ON COLUMN 発注データ明細.発注単価 IS '発注時の単価（履歴保持）';
COMMENT ON COLUMN 発注データ明細.発注数量 IS '発注した数量';
COMMENT ON COLUMN 発注データ明細.入荷予定日 IS '仕入先からの入荷予定日';
COMMENT ON COLUMN 発注データ明細.入荷済数量 IS '実際に入荷した数量';
COMMENT ON COLUMN 発注データ明細.完了フラグ IS '発注完了フラグ（0=未完了、1=完了）';

CREATE INDEX idx_po_detail_product ON 発注データ明細(商品コード);
