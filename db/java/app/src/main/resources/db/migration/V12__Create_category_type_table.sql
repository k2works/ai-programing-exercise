-- 取引先分類種別マスタ
-- 目的: 取引先の分類の「種類」を定義（業種、地域、取引規模など）
CREATE TABLE 取引先分類種別マスタ (
    取引先分類種別コード VARCHAR(2) PRIMARY KEY,
    取引先分類種別名 VARCHAR(20),
    作成日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    作成者名 VARCHAR(100),
    更新日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    更新者名 VARCHAR(100)
);

COMMENT ON TABLE 取引先分類種別マスタ IS '取引先分類の種別を定義するマスタ';
COMMENT ON COLUMN 取引先分類種別マスタ.取引先分類種別コード IS '分類種別を一意に識別するコード';
COMMENT ON COLUMN 取引先分類種別マスタ.取引先分類種別名 IS '分類種別の名称（例：業種、地域）';
