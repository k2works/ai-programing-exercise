-- 受注明細テーブル
CREATE TABLE 受注明細 (
    受注番号 VARCHAR(20) NOT NULL,
    明細番号 INTEGER NOT NULL,
    商品コード VARCHAR(16) NOT NULL,
    商品名 VARCHAR(10) NOT NULL,
    販売単価 INTEGER DEFAULT 0,
    数量 INTEGER DEFAULT 1,
    消費税率 INTEGER DEFAULT 0,
    引当数量 INTEGER DEFAULT 0,
    出荷指示数量 INTEGER DEFAULT 0,
    出荷済数量 INTEGER DEFAULT 0,
    完了フラグ INTEGER DEFAULT 0,
    値引額 INTEGER DEFAULT 0,
    納品日 TIMESTAMP,
    作成日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    作成者名 VARCHAR(12),
    更新日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    更新者名 VARCHAR(12),
    PRIMARY KEY (受注番号, 明細番号),
    CONSTRAINT fk_order_detail_order FOREIGN KEY (受注番号)
        REFERENCES 受注(受注番号) ON DELETE CASCADE,
    CONSTRAINT fk_order_detail_product FOREIGN KEY (商品コード)
        REFERENCES 商品マスタ(商品コード)
);

-- インデックス
CREATE INDEX idx_order_detail_product ON 受注明細(商品コード);

-- コメント
COMMENT ON TABLE 受注明細 IS '受注の商品明細を管理するテーブル';
COMMENT ON COLUMN 受注明細.明細番号 IS '同一受注内での明細の連番';
COMMENT ON COLUMN 受注明細.引当数量 IS '在庫から引き当てた数量';
COMMENT ON COLUMN 受注明細.出荷指示数量 IS '出荷を指示した数量';
COMMENT ON COLUMN 受注明細.出荷済数量 IS '実際に出荷が完了した数量';
COMMENT ON COLUMN 受注明細.完了フラグ IS '明細が完了したかどうか（0:未完了、1:完了）';
