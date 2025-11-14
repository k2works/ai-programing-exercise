-- 商品マスタテーブル作成
CREATE TABLE IF NOT EXISTS 商品マスタ (
    商品コード VARCHAR(16) PRIMARY KEY,
    商品正式名 VARCHAR(40) NOT NULL,
    商品略称 VARCHAR(10) NOT NULL,
    商品名カナ VARCHAR(20),
    商品区分 VARCHAR(1),
    製品型番 VARCHAR(40),
    販売単価 INTEGER NOT NULL DEFAULT 0,
    仕入単価 INTEGER DEFAULT 0,
    売上原価 INTEGER NOT NULL DEFAULT 0,
    税区分 INTEGER NOT NULL DEFAULT 1,
    商品分類コード VARCHAR(8),
    雑区分 INTEGER,
    在庫管理対象区分 INTEGER DEFAULT 1,
    在庫引当区分 INTEGER,
    仕入先コード VARCHAR(8),
    仕入先枝番 INTEGER,
    作成日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    作成者名 VARCHAR(12),
    更新日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    更新者名 VARCHAR(12)
);

-- インデックス作成
CREATE INDEX IF NOT EXISTS idx_product_category ON 商品マスタ(商品分類コード);
CREATE INDEX IF NOT EXISTS idx_product_supplier ON 商品マスタ(仕入先コード, 仕入先枝番);
CREATE INDEX IF NOT EXISTS idx_product_name ON 商品マスタ(商品略称);

-- コメント追加
COMMENT ON TABLE 商品マスタ IS '商品の基本情報を管理するマスタテーブル';
COMMENT ON COLUMN 商品マスタ.商品コード IS '商品を一意に識別するコード';
COMMENT ON COLUMN 商品マスタ.商品正式名 IS '商品の正式な名称';
COMMENT ON COLUMN 商品マスタ.商品略称 IS '商品の略称';
COMMENT ON COLUMN 商品マスタ.販売単価 IS '標準の販売単価';
COMMENT ON COLUMN 商品マスタ.仕入単価 IS '仕入時の単価';
COMMENT ON COLUMN 商品マスタ.税区分 IS '課税区分（1:課税、2:非課税、3:免税）';
COMMENT ON COLUMN 商品マスタ.在庫管理対象区分 IS '在庫を管理するかどうか（0:管理しない、1:管理する）';
COMMENT ON COLUMN 商品マスタ.在庫引当区分 IS '在庫を引当するかどうか（0:引当しない、1:引当する）';
