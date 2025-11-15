-- 商品分類マスタテーブル
CREATE TABLE 商品分類マスタ (
    商品分類コード VARCHAR(8) PRIMARY KEY,
    商品分類名 VARCHAR(30) NOT NULL,
    商品分類階層 INTEGER NOT NULL DEFAULT 0,
    商品分類パス VARCHAR(100) NOT NULL,
    最下層区分 INTEGER NOT NULL DEFAULT 0,
    作成日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    作成者名 VARCHAR(12) NOT NULL,
    更新日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    更新者名 VARCHAR(12) NOT NULL
);

-- インデックス
CREATE INDEX idx_product_category_layer ON 商品分類マスタ(商品分類階層);
CREATE INDEX idx_product_category_path ON 商品分類マスタ(商品分類パス);

-- コメント
COMMENT ON TABLE 商品分類マスタ IS '商品の階層的な分類を管理するマスタテーブル';
COMMENT ON COLUMN 商品分類マスタ.商品分類コード IS '商品分類を一意に識別するコード';
COMMENT ON COLUMN 商品分類マスタ.商品分類名 IS '商品分類名';
COMMENT ON COLUMN 商品分類マスタ.商品分類階層 IS '分類の階層レベル（1が最上位）';
COMMENT ON COLUMN 商品分類マスタ.商品分類パス IS '階層構造を表すパス（例: /PC/PC01/）';
COMMENT ON COLUMN 商品分類マスタ.最下層区分 IS '最下層かどうか（1:最下層、0:中間層）';

-- 商品マスタに外部キー制約を追加
ALTER TABLE 商品マスタ
ADD CONSTRAINT fk_product_category 
FOREIGN KEY (商品分類コード) REFERENCES 商品分類マスタ(商品分類コード);
