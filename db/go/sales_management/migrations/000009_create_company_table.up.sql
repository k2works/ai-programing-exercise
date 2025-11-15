-- 取引先マスタテーブル
CREATE TABLE 取引先マスタ (
    取引先コード VARCHAR(8) PRIMARY KEY,
    取引先名 VARCHAR(40) NOT NULL,
    取引先名カナ VARCHAR(40),
    仕入先区分 INTEGER DEFAULT 0,
    郵便番号 CHAR(8),
    都道府県 VARCHAR(4),
    住所１ VARCHAR(40),
    住所２ VARCHAR(40),
    取引禁止フラグ INTEGER DEFAULT 0,
    雑区分 INTEGER DEFAULT 0,
    取引先グループコード VARCHAR(4) NOT NULL,
    与信限度額 INTEGER DEFAULT 0,
    与信一時増加枠 INTEGER DEFAULT 0,
    作成日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    作成者名 VARCHAR(12),
    更新日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    更新者名 VARCHAR(12),
    CONSTRAINT fk_company_group FOREIGN KEY (取引先グループコード)
        REFERENCES 取引先グループマスタ(取引先グループコード)
);

-- インデックス
CREATE INDEX idx_company_group ON 取引先マスタ(取引先グループコード);
CREATE INDEX idx_company_name ON 取引先マスタ(取引先名);

-- コメント
COMMENT ON TABLE 取引先マスタ IS 'すべての取引先の基本情報を管理するマスタテーブル';
COMMENT ON COLUMN 取引先マスタ.取引先コード IS '取引先を一意に識別するコード';
COMMENT ON COLUMN 取引先マスタ.仕入先区分 IS '仕入先としての種別(0:通常、1:メーカー、2:商社)';
COMMENT ON COLUMN 取引先マスタ.取引禁止フラグ IS '取引を禁止するかどうか(0:許可、1:禁止)';
COMMENT ON COLUMN 取引先マスタ.与信限度額 IS '与信の上限金額';
