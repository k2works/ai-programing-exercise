-- 商品マスタテーブル
CREATE TABLE 商品マスタ (
    商品コード VARCHAR(16) PRIMARY KEY,
    商品正式名 VARCHAR(40) NOT NULL,
    商品略称 VARCHAR(10) NOT NULL,
    商品名カナ VARCHAR(80),
    商品区分 VARCHAR(8),
    製品型番 VARCHAR(30),
    販売単価 INTEGER NOT NULL DEFAULT 0,
    仕入単価 INTEGER NOT NULL DEFAULT 0,
    売上原価 INTEGER NOT NULL DEFAULT 0,
    税区分 INTEGER NOT NULL DEFAULT 1,
    商品分類コード VARCHAR(8),
    雑区分 INTEGER DEFAULT 0,
    在庫管理対象区分 INTEGER NOT NULL DEFAULT 1,
    在庫引当区分 INTEGER DEFAULT 0,
    仕入先コード VARCHAR(8),
    仕入先枝番 INTEGER,
    作成日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    作成者名 VARCHAR(12) NOT NULL,
    更新日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    更新者名 VARCHAR(12) NOT NULL
);

-- インデックス
CREATE INDEX idx_product_name ON 商品マスタ(商品略称);
CREATE INDEX idx_product_category ON 商品マスタ(商品分類コード);
CREATE INDEX idx_product_supplier ON 商品マスタ(仕入先コード, 仕入先枝番);

-- コメント
COMMENT ON TABLE 商品マスタ IS '商品の基本情報を管理するマスタテーブル';
COMMENT ON COLUMN 商品マスタ.商品コード IS '商品を一意に識別するコード';
COMMENT ON COLUMN 商品マスタ.商品正式名 IS '商品の正式名称';
COMMENT ON COLUMN 商品マスタ.商品略称 IS '商品の略称';
COMMENT ON COLUMN 商品マスタ.販売単価 IS '標準販売単価';
COMMENT ON COLUMN 商品マスタ.仕入単価 IS '標準仕入単価';
COMMENT ON COLUMN 商品マスタ.税区分 IS '税区分（1:課税、2:非課税、3:免税）';
COMMENT ON COLUMN 商品マスタ.在庫管理対象区分 IS '在庫管理の対象とするか（0:対象外、1:対象）';
