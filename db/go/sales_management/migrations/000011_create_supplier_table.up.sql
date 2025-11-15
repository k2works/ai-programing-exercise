-- 仕入先マスタテーブル
CREATE TABLE 仕入先マスタ (
    仕入先コード VARCHAR(8) NOT NULL,
    仕入先枝番 INTEGER NOT NULL,
    仕入先区分 INTEGER DEFAULT 0,
    仕入先名 VARCHAR(40),
    仕入先名カナ VARCHAR(40),
    担当社員コード VARCHAR(10),
    仕入先担当者名 VARCHAR(40),
    仕入先担当部署名 VARCHAR(40),
    仕入先郵便番号 CHAR(8),
    仕入先都道府県 VARCHAR(4),
    仕入先住所１ VARCHAR(40),
    仕入先電話番号 VARCHAR(13),
    仕入先メールアドレス VARCHAR(40),
    仕入先締日 INTEGER,
    仕入先支払月数 INTEGER,
    仕入先支払日 INTEGER,
    仕入先支払方法 INTEGER,
    作成日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    作成者名 VARCHAR(12),
    更新日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    更新者名 VARCHAR(12),
    PRIMARY KEY (仕入先コード, 仕入先枝番),
    CONSTRAINT fk_supplier_company FOREIGN KEY (仕入先コード)
        REFERENCES 取引先マスタ(取引先コード) ON DELETE CASCADE
);

-- インデックス
CREATE INDEX idx_supplier_company ON 仕入先マスタ(仕入先コード);

-- コメント
COMMENT ON TABLE 仕入先マスタ IS '仕入先としての詳細情報を管理するテーブル';
COMMENT ON COLUMN 仕入先マスタ.仕入先コード IS '取引先マスタへの外部キー';
COMMENT ON COLUMN 仕入先マスタ.仕入先枝番 IS '同一取引先内での枝番';
