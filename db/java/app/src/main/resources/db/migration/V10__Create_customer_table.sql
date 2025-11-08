-- 顧客マスタ
-- 目的: 取引先の「顧客」としての詳細情報を管理
-- 特徴: 取引先コード+枝番の複合主キー、請求・支払条件を保持
CREATE TABLE 顧客マスタ (
    顧客コード VARCHAR(8) NOT NULL,
    顧客枝番 INTEGER NOT NULL,
    顧客区分 INTEGER DEFAULT 0,
    請求先コード VARCHAR(8) NOT NULL,
    請求先枝番 INTEGER,
    回収先コード VARCHAR(8) NOT NULL,
    回収先枝番 INTEGER,
    顧客名 VARCHAR(40) NOT NULL,
    顧客名カナ VARCHAR(40),
    自社担当者コード VARCHAR(10) NOT NULL,
    顧客担当者名 VARCHAR(20),
    顧客部門名 VARCHAR(40),
    顧客郵便番号 CHAR(8),
    顧客都道府県 VARCHAR(4),
    顧客住所１ VARCHAR(40),
    顧客住所２ VARCHAR(40),
    顧客電話番号 VARCHAR(13),
    顧客ＦＡＸ番号 VARCHAR(13),
    顧客メールアドレス VARCHAR(100),
    顧客請求区分 INTEGER DEFAULT 0,
    顧客締日１ INTEGER NOT NULL,
    顧客支払月１ INTEGER DEFAULT 1,
    顧客支払日１ INTEGER,
    顧客支払方法１ INTEGER DEFAULT 1,
    顧客締日２ INTEGER NOT NULL,
    顧客支払月２ INTEGER DEFAULT 1,
    顧客支払日２ INTEGER,
    顧客支払方法２ INTEGER DEFAULT 1,
    作成日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    作成者名 VARCHAR(100),
    更新日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    更新者名 VARCHAR(100),

    PRIMARY KEY (顧客コード, 顧客枝番),

    CONSTRAINT fk_customer_company FOREIGN KEY (顧客コード)
        REFERENCES 取引先マスタ(取引先コード),
    CONSTRAINT fk_customer_employee FOREIGN KEY (自社担当者コード)
        REFERENCES 社員マスタ(社員コード)
);

COMMENT ON TABLE 顧客マスタ IS '取引先の顧客としての詳細情報';
COMMENT ON COLUMN 顧客マスタ.顧客コード IS '取引先コードへの外部キー';
COMMENT ON COLUMN 顧客マスタ.顧客枝番 IS '同一取引先の複数顧客を区別する枝番';
COMMENT ON COLUMN 顧客マスタ.請求先コード IS '請求書送付先の顧客コード';
COMMENT ON COLUMN 顧客マスタ.回収先コード IS '代金回収先の顧客コード';
COMMENT ON COLUMN 顧客マスタ.顧客締日１ IS '第1締日（月の日付）';
COMMENT ON COLUMN 顧客マスタ.顧客支払月１ IS '第1締日の支払月（当月=0、翌月=1、翌々月=2）';
COMMENT ON COLUMN 顧客マスタ.顧客支払日１ IS '第1締日の支払日';

CREATE INDEX idx_customer_company ON 顧客マスタ(顧客コード);
CREATE INDEX idx_customer_employee ON 顧客マスタ(自社担当者コード);
