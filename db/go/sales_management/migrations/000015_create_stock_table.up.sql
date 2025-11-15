-- 在庫テーブル
CREATE TABLE 在庫 (
    倉庫コード VARCHAR(3) NOT NULL,
    商品コード VARCHAR(16) NOT NULL,
    ロット番号 VARCHAR(20) NOT NULL,
    在庫区分 VARCHAR(1) NOT NULL,
    品質区分 VARCHAR(1) NOT NULL,
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

-- インデックス
CREATE INDEX idx_stock_warehouse ON 在庫(倉庫コード);
CREATE INDEX idx_stock_product ON 在庫(商品コード);
CREATE INDEX idx_stock_lot ON 在庫(ロット番号);

-- コメント
COMMENT ON TABLE 在庫 IS '倉庫ご と、商品ごと、ロットごとの在庫数を管理するテーブル';
COMMENT ON COLUMN 在庫.倉庫コード IS '在庫を保管している倉庫のコード';
COMMENT ON COLUMN 在庫.商品コード IS '在庫商品のコード';
COMMENT ON COLUMN 在庫.ロット番号 IS '在庫のロット番号（トレーサビリティ管理）';
COMMENT ON COLUMN 在庫.在庫区分 IS '在庫の区分（1:通常在庫、2:預かり在庫、3:委託在庫）';
COMMENT ON COLUMN 在庫.品質区分 IS '在庫の品質区分（A:良品、B:不良品、C:検査中）';
COMMENT ON COLUMN 在庫.実在庫数 IS '物理的に存在する在庫数';
COMMENT ON COLUMN 在庫.有効在庫数 IS '販売可能な在庫数（実在庫数 - 引当数）';
COMMENT ON COLUMN 在庫.最終出荷日 IS '最後に出荷された日時';
