-- 倉庫マスタテーブル
CREATE TABLE 倉庫マスタ (
    倉庫コード VARCHAR(3) PRIMARY KEY,
    倉庫名 VARCHAR(40) NOT NULL,
    郵便番号 VARCHAR(8),
    都道府県 VARCHAR(4),
    住所１ VARCHAR(40),
    住所２ VARCHAR(40),
    電話番号 VARCHAR(15),
    FAX番号 VARCHAR(15),
    作成日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    作成者名 VARCHAR(12),
    更新日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    更新者名 VARCHAR(12)
);

-- インデックス
CREATE INDEX idx_warehouse_name ON 倉庫マスタ(倉庫名);

-- コメント
COMMENT ON TABLE 倉庫マスタ IS '商品を保管する倉庫の情報を管理するマスタテーブル';
COMMENT ON COLUMN 倉庫マスタ.倉庫コード IS '倉庫を一意に識別するコード';
COMMENT ON COLUMN 倉庫マスタ.倉庫名 IS '倉庫の名称';
COMMENT ON COLUMN 倉庫マスタ.郵便番号 IS '倉庫の郵便番号';
COMMENT ON COLUMN 倉庫マスタ.都道府県 IS '倉庫の所在地（都道府県）';
COMMENT ON COLUMN 倉庫マスタ.住所１ IS '倉庫の住所（市区町村以降）';
COMMENT ON COLUMN 倉庫マスタ.住所２ IS '倉庫の住所（建物名等）';
