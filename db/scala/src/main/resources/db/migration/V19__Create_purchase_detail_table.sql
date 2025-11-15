-- 仕入明細テーブルの作成
CREATE TABLE IF NOT EXISTS 仕入明細 (
  仕入番号 VARCHAR(13) NOT NULL,
  仕入明細番号 INTEGER NOT NULL,
  商品コード VARCHAR(16) NOT NULL,
  ロット番号 VARCHAR(20) NOT NULL,
  数量 INTEGER NOT NULL DEFAULT 1,
  単価 INTEGER NOT NULL DEFAULT 0,
  作成日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  作成者名 VARCHAR(12),
  更新日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  更新者名 VARCHAR(12),
  PRIMARY KEY (仕入番号, 仕入明細番号),
  CONSTRAINT fk_purchase_detail_purchase FOREIGN KEY (仕入番号)
    REFERENCES 仕入(仕入番号) ON DELETE CASCADE,
  CONSTRAINT fk_purchase_detail_product FOREIGN KEY (商品コード)
    REFERENCES 商品マスタ(商品コード)
);

-- インデックスの作成
CREATE INDEX idx_purchase_detail_product ON 仕入明細(商品コード);
CREATE INDEX idx_purchase_detail_lot ON 仕入明細(ロット番号);

-- コメントの追加
COMMENT ON TABLE 仕入明細 IS '仕入の明細情報を管理するテーブル';
COMMENT ON COLUMN 仕入明細.仕入番号 IS '仕入ヘッダーの仕入番号';
COMMENT ON COLUMN 仕入明細.仕入明細番号 IS '仕入内での明細の連番';
COMMENT ON COLUMN 仕入明細.商品コード IS '仕入した商品のコード';
COMMENT ON COLUMN 仕入明細.ロット番号 IS '仕入商品のロット番号（トレーサビリティ管理）';
COMMENT ON COLUMN 仕入明細.数量 IS '仕入数量';
COMMENT ON COLUMN 仕入明細.単価 IS '仕入時の単価';
