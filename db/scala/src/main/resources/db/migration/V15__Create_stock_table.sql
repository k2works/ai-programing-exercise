-- 在庫テーブルの作成
CREATE TABLE IF NOT EXISTS 在庫 (
  倉庫コード VARCHAR(3) NOT NULL,
  商品コード VARCHAR(16) NOT NULL,
  ロット番号 VARCHAR(20) NOT NULL,
  在庫区分 VARCHAR(1) NOT NULL DEFAULT '1',
  品質区分 VARCHAR(1) NOT NULL DEFAULT 'G',
  実在庫数 INTEGER NOT NULL DEFAULT 0,
  有効在庫数 INTEGER NOT NULL DEFAULT 0,
  最終出荷日 TIMESTAMP,
  作成日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  作成者名 VARCHAR(12),
  更新日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  更新者名 VARCHAR(12),
  PRIMARY KEY (倉庫コード, 商品コード, ロット番号, 在庫区分, 品質区分),
  CONSTRAINT fk_stock_warehouse FOREIGN KEY (倉庫コード)
    REFERENCES 倉庫マスタ(倉庫コード),
  CONSTRAINT fk_stock_product FOREIGN KEY (商品コード)
    REFERENCES 商品マスタ(商品コード)
);

-- インデックスの作成
CREATE INDEX idx_stock_warehouse ON 在庫(倉庫コード);
CREATE INDEX idx_stock_product ON 在庫(商品コード);
CREATE INDEX idx_stock_lot ON 在庫(ロット番号);

-- コメントの追加
COMMENT ON TABLE 在庫 IS '倉庫ごとの在庫数を管理するトランザクションテーブル';
COMMENT ON COLUMN 在庫.倉庫コード IS '倉庫を識別するコード';
COMMENT ON COLUMN 在庫.商品コード IS '商品を識別するコード';
COMMENT ON COLUMN 在庫.ロット番号 IS '商品の仕入・製造単位を表すロット番号';
COMMENT ON COLUMN 在庫.在庫区分 IS '在庫の種別（1:正常在庫、2:預かり在庫、3:不良在庫）';
COMMENT ON COLUMN 在庫.品質区分 IS '良品・不良品の区分（G:良品、B:不良品）';
COMMENT ON COLUMN 在庫.実在庫数 IS '物理的に存在する在庫数';
COMMENT ON COLUMN 在庫.有効在庫数 IS '引当可能な在庫数（実在庫 - 引当済）';
COMMENT ON COLUMN 在庫.最終出荷日 IS '最後に出荷した日時';
