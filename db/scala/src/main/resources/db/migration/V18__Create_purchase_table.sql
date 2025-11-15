-- 仕入テーブルの作成
CREATE TABLE IF NOT EXISTS 仕入 (
  仕入番号 VARCHAR(13) PRIMARY KEY,
  仕入日 TIMESTAMP NOT NULL,
  発注番号 VARCHAR(13) NOT NULL,
  倉庫コード VARCHAR(3) NOT NULL,
  完了フラグ INTEGER NOT NULL DEFAULT 0,
  作成日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  作成者名 VARCHAR(12),
  更新日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  更新者名 VARCHAR(12),
  CONSTRAINT fk_purchase_po FOREIGN KEY (発注番号)
    REFERENCES 発注(発注番号),
  CONSTRAINT fk_purchase_warehouse FOREIGN KEY (倉庫コード)
    REFERENCES 倉庫マスタ(倉庫コード)
);

-- インデックスの作成
CREATE INDEX idx_purchase_po ON 仕入(発注番号);
CREATE INDEX idx_purchase_date ON 仕入(仕入日);
CREATE INDEX idx_purchase_warehouse ON 仕入(倉庫コード);

-- コメントの追加
COMMENT ON TABLE 仕入 IS '商品の仕入情報を管理するヘッダーテーブル';
COMMENT ON COLUMN 仕入.仕入番号 IS '仕入を一意に識別する番号';
COMMENT ON COLUMN 仕入.仕入日 IS '仕入を行った日時';
COMMENT ON COLUMN 仕入.発注番号 IS '関連する発注の番号';
COMMENT ON COLUMN 仕入.倉庫コード IS '仕入先倉庫のコード';
COMMENT ON COLUMN 仕入.完了フラグ IS '仕入処理が完了したかどうか（0:未完了、1:完了）';
