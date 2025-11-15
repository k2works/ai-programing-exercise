-- 発注明細テーブルの作成
CREATE TABLE IF NOT EXISTS 発注明細 (
  発注番号 VARCHAR(13) NOT NULL,
  発注明細番号 INTEGER NOT NULL,
  商品コード VARCHAR(16) NOT NULL,
  数量 INTEGER NOT NULL DEFAULT 1,
  単価 INTEGER NOT NULL DEFAULT 0,
  作成日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  作成者名 VARCHAR(12),
  更新日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  更新者名 VARCHAR(12),
  PRIMARY KEY (発注番号, 発注明細番号),
  CONSTRAINT fk_po_detail_po FOREIGN KEY (発注番号)
    REFERENCES 発注(発注番号) ON DELETE CASCADE,
  CONSTRAINT fk_po_detail_product FOREIGN KEY (商品コード)
    REFERENCES 商品マスタ(商品コード)
);

-- インデックスの作成
CREATE INDEX idx_po_detail_product ON 発注明細(商品コード);

-- コメントの追加
COMMENT ON TABLE 発注明細 IS '発注の明細情報を管理するテーブル';
COMMENT ON COLUMN 発注明細.発注番号 IS '発注ヘッダーの発注番号';
COMMENT ON COLUMN 発注明細.発注明細番号 IS '発注内での明細の連番';
COMMENT ON COLUMN 発注明細.商品コード IS '発注する商品のコード';
COMMENT ON COLUMN 発注明細.数量 IS '発注数量';
COMMENT ON COLUMN 発注明細.単価 IS '発注時の単価';
