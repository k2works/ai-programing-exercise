-- 売上テーブルの作成
CREATE TABLE IF NOT EXISTS 売上 (
  売上番号 VARCHAR(13) PRIMARY KEY,
  売上日 TIMESTAMP,
  売上区分 INTEGER NOT NULL DEFAULT 1,
  受注番号 VARCHAR(10),
  部門コード VARCHAR(6) NOT NULL,
  開始日 TIMESTAMP NOT NULL,
  取引先コード VARCHAR(8) NOT NULL,
  売上金額 INTEGER NOT NULL DEFAULT 0,
  消費税 INTEGER NOT NULL DEFAULT 0,
  訂正番号 VARCHAR(13),
  元伝票番号 VARCHAR(13),
  作成日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  作成者名 VARCHAR(12),
  更新日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  更新者名 VARCHAR(12),
  CONSTRAINT fk_sales_order FOREIGN KEY (受注番号)
    REFERENCES 受注(受注番号),
  CONSTRAINT fk_sales_department FOREIGN KEY (部門コード, 開始日)
    REFERENCES 部門マスタ(部門コード, 開始日),
  CONSTRAINT fk_sales_company FOREIGN KEY (取引先コード)
    REFERENCES 取引先マスタ(取引先コード)
);

-- インデックスの作成
CREATE INDEX idx_sales_order ON 売上(受注番号);
CREATE INDEX idx_sales_date ON 売上(売上日);
CREATE INDEX idx_sales_company ON 売上(取引先コード);
CREATE INDEX idx_sales_department ON 売上(部門コード, 開始日);

-- コメントの追加
COMMENT ON TABLE 売上 IS '商品の売上情報を管理するヘッダーテーブル';
COMMENT ON COLUMN 売上.売上番号 IS '売上を一意に識別する番号';
COMMENT ON COLUMN 売上.売上区分 IS '売上の種類（1:通常売上、2:返品、3:値引）';
COMMENT ON COLUMN 売上.訂正番号 IS '訂正伝票の売上番号（赤黒伝票）';
COMMENT ON COLUMN 売上.元伝票番号 IS '訂正元の売上番号（赤黒伝票）';
