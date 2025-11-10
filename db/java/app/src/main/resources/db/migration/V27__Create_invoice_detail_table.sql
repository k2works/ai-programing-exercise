-- 請求データ明細
-- 目的: 請求の売上明細別情報を管理
-- 特徴: 請求番号+行番号の複合主キー、売上明細への参照
CREATE TABLE 請求データ明細 (
    請求番号 VARCHAR(10) NOT NULL,
    請求行番号 INTEGER NOT NULL,
    売上番号 VARCHAR(10) NOT NULL,
    売上行番号 INTEGER NOT NULL,
    請求金額 INTEGER DEFAULT 0,
    作成日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    作成者名 VARCHAR(100),
    更新日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    更新者名 VARCHAR(100),

    PRIMARY KEY (請求番号, 請求行番号),

    CONSTRAINT fk_invoice_detail_invoice FOREIGN KEY (請求番号)
        REFERENCES 請求データ(請求番号) ON DELETE CASCADE,
    CONSTRAINT fk_invoice_detail_sales FOREIGN KEY (売上番号, 売上行番号)
        REFERENCES 売上データ明細(売上番号, 売上行番号)
);

COMMENT ON TABLE 請求データ明細 IS '請求の売上明細別情報';
COMMENT ON COLUMN 請求データ明細.請求番号 IS '親の請求データへの外部キー';
COMMENT ON COLUMN 請求データ明細.請求行番号 IS '請求内での明細行番号';
COMMENT ON COLUMN 請求データ明細.売上番号 IS '売上データの参照';
COMMENT ON COLUMN 請求データ明細.売上行番号 IS '売上明細の参照';
COMMENT ON COLUMN 請求データ明細.請求金額 IS '請求する金額';

CREATE INDEX idx_invoice_detail_sales ON 請求データ明細(売上番号, 売上行番号);
