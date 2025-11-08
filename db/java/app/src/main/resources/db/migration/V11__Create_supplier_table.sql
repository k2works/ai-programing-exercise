-- 仕入先マスタ
-- 目的: 取引先の「仕入先」としての詳細情報を管理
-- 特徴: 取引先コード+枝番の複合主キー、支払条件を保持
CREATE TABLE 仕入先マスタ (
    仕入先コード VARCHAR(8) NOT NULL,
    仕入先枝番 INTEGER NOT NULL,
    仕入先名 VARCHAR(40) NOT NULL,
    仕入先名カナ VARCHAR(40),
    仕入先担当者名 VARCHAR(20),
    仕入先部門名 VARCHAR(40),
    仕入先郵便番号 CHAR(8),
    仕入先都道府県 VARCHAR(4),
    仕入先住所１ VARCHAR(40),
    仕入先住所２ VARCHAR(40),
    仕入先電話番号 VARCHAR(13),
    仕入先ＦＡＸ番号 VARCHAR(13),
    仕入先メールアドレス VARCHAR(100),
    仕入先締日 INTEGER NOT NULL,
    仕入先支払月 INTEGER DEFAULT 1,
    仕入先支払日 INTEGER,
    仕入先支払方法 INTEGER DEFAULT 1,
    作成日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    作成者名 VARCHAR(100),
    更新日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    更新者名 VARCHAR(100),

    PRIMARY KEY (仕入先コード, 仕入先枝番),

    CONSTRAINT fk_supplier_company FOREIGN KEY (仕入先コード)
        REFERENCES 取引先マスタ(取引先コード)
);

COMMENT ON TABLE 仕入先マスタ IS '取引先の仕入先としての詳細情報';
COMMENT ON COLUMN 仕入先マスタ.仕入先コード IS '取引先コードへの外部キー';
COMMENT ON COLUMN 仕入先マスタ.仕入先枝番 IS '同一取引先の複数仕入先を区別する枝番';
COMMENT ON COLUMN 仕入先マスタ.仕入先締日 IS '締日（月の日付）';
COMMENT ON COLUMN 仕入先マスタ.仕入先支払月 IS '支払月（当月=0、翌月=1、翌々月=2）';

CREATE INDEX idx_supplier_company ON 仕入先マスタ(仕入先コード);
