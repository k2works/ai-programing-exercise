-- 売上データ明細
-- 目的: 売上の商品別明細情報を管理
-- 特徴: 売上番号+行番号の複合主キー、請求との連携
CREATE TABLE 売上データ明細 (
    売上番号 VARCHAR(10) NOT NULL,
    売上行番号 INTEGER NOT NULL,
    商品コード VARCHAR(20) NOT NULL,
    商品名 VARCHAR(200) NOT NULL,
    販売単価 INTEGER DEFAULT 0,
    出荷数量 INTEGER DEFAULT 0,
    売上数量 INTEGER DEFAULT 1,
    値引金額 INTEGER DEFAULT 0,
    請求日 TIMESTAMP,
    請求番号 VARCHAR(10),
    請求遅延区分 INTEGER,
    自動仕訳日 TIMESTAMP,
    作成日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    作成者名 VARCHAR(100),
    更新日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    更新者名 VARCHAR(100),

    PRIMARY KEY (売上番号, 売上行番号),

    CONSTRAINT fk_sales_detail_sales FOREIGN KEY (売上番号)
        REFERENCES 売上データ(売上番号) ON DELETE CASCADE,
    CONSTRAINT fk_sales_detail_product FOREIGN KEY (商品コード)
        REFERENCES 商品マスタ(商品コード)
);

COMMENT ON TABLE 売上データ明細 IS '売上の商品別明細情報';
COMMENT ON COLUMN 売上データ明細.売上番号 IS '親の売上データへの外部キー';
COMMENT ON COLUMN 売上データ明細.売上行番号 IS '売上内での明細行番号';
COMMENT ON COLUMN 売上データ明細.商品コード IS '売上商品';
COMMENT ON COLUMN 売上データ明細.商品名 IS '売上時の商品名（履歴保持）';
COMMENT ON COLUMN 売上データ明細.販売単価 IS '売上時の販売単価（履歴保持）';
COMMENT ON COLUMN 売上データ明細.出荷数量 IS '実際に出荷した数量';
COMMENT ON COLUMN 売上データ明細.売上数量 IS '売上として計上する数量';
COMMENT ON COLUMN 売上データ明細.請求日 IS '請求書に計上した日';
COMMENT ON COLUMN 売上データ明細.請求番号 IS '計上した請求書の番号';
COMMENT ON COLUMN 売上データ明細.請求遅延区分 IS '請求遅延の理由区分';

CREATE INDEX idx_sales_detail_product ON 売上データ明細(商品コード);
CREATE INDEX idx_sales_detail_invoice ON 売上データ明細(請求番号);
