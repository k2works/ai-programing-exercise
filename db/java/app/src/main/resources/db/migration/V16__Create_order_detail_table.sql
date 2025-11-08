-- 受注データ明細
-- 目的: 受注の商品別明細情報を管理
-- 特徴: 受注番号+行番号の複合主キー、商品マスタへの外部キー
CREATE TABLE 受注データ明細 (
    受注番号 VARCHAR(10) NOT NULL,
    受注行番号 INTEGER NOT NULL,
    商品コード VARCHAR(20) NOT NULL,
    商品名 VARCHAR(200) NOT NULL,
    販売単価 INTEGER DEFAULT 0,
    受注数量 INTEGER DEFAULT 1,
    消費税率 INTEGER DEFAULT 0,
    引当数量 INTEGER DEFAULT 0,
    出荷指示数量 INTEGER DEFAULT 0,
    出荷済数量 INTEGER DEFAULT 0,
    完了フラグ INTEGER DEFAULT 0,
    値引金額 INTEGER DEFAULT 0,
    納期 TIMESTAMP,
    作成日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    作成者名 VARCHAR(100),
    更新日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    更新者名 VARCHAR(100),

    PRIMARY KEY (受注番号, 受注行番号),

    CONSTRAINT fk_order_detail_order FOREIGN KEY (受注番号)
        REFERENCES 受注データ(受注番号) ON DELETE CASCADE,
    CONSTRAINT fk_order_detail_product FOREIGN KEY (商品コード)
        REFERENCES 商品マスタ(商品コード)
);

COMMENT ON TABLE 受注データ明細 IS '受注の商品別明細情報';
COMMENT ON COLUMN 受注データ明細.受注番号 IS '親の受注データへの外部キー';
COMMENT ON COLUMN 受注データ明細.受注行番号 IS '受注内での明細行番号（1から開始）';
COMMENT ON COLUMN 受注データ明細.商品コード IS '受注した商品';
COMMENT ON COLUMN 受注データ明細.商品名 IS '受注時の商品名（履歴保持）';
COMMENT ON COLUMN 受注データ明細.販売単価 IS '受注時の販売単価（履歴保持）';
COMMENT ON COLUMN 受注データ明細.受注数量 IS '注文された数量';
COMMENT ON COLUMN 受注データ明細.引当数量 IS '在庫から引き当てた数量';
COMMENT ON COLUMN 受注データ明細.出荷指示数量 IS '出荷指示済みの数量';
COMMENT ON COLUMN 受注データ明細.出荷済数量 IS '実際に出荷完了した数量';
COMMENT ON COLUMN 受注データ明細.完了フラグ IS '明細の処理完了フラグ（0=未完了、1=完了）';

CREATE INDEX idx_order_detail_product ON 受注データ明細(商品コード);
