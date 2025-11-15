-- 売上明細テーブル
CREATE TABLE 売上明細 (
    売上番号 VARCHAR(20) NOT NULL,
    明細番号 INTEGER NOT NULL,
    商品コード VARCHAR(16) NOT NULL,
    商品名 VARCHAR(20),
    販売単価 INTEGER NOT NULL DEFAULT 0,
    出荷済数量 INTEGER NOT NULL DEFAULT 0,
    数量 INTEGER NOT NULL DEFAULT 0,
    値引額 INTEGER NOT NULL DEFAULT 0,
    請求日 TIMESTAMP,
    請求番号 VARCHAR(10),
    請求遅延区分 INTEGER,
    自動仕訳日 TIMESTAMP,
    作成日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    作成者名 VARCHAR(12),
    更新日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    更新者名 VARCHAR(12),
    PRIMARY KEY (売上番号, 明細番号),
    CONSTRAINT fk_sales_detail_sales FOREIGN KEY (売上番号)
        REFERENCES 売上(売上番号) ON DELETE CASCADE,
    CONSTRAINT fk_sales_detail_product FOREIGN KEY (商品コード)
        REFERENCES 商品マスタ(商品コード)
);

-- インデックス
CREATE INDEX idx_sales_detail_product ON 売上明細(商品コード);
CREATE INDEX idx_sales_detail_invoice ON 売上明細(請求番号);

-- コメント
COMMENT ON TABLE 売上明細 IS '売上の明細情報を管理するテーブル';
COMMENT ON COLUMN 売上明細.明細番号 IS '売上内での明細の連番';
COMMENT ON COLUMN 売上明細.出荷済数量 IS '実際に出荷した数量';
COMMENT ON COLUMN 売上明細.請求遅延区分 IS '請求の遅延状況区分';
