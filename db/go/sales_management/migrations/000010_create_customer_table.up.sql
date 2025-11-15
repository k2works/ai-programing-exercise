-- 顧客マスタテーブル
CREATE TABLE 顧客マスタ (
    顧客コード VARCHAR(8) NOT NULL,
    顧客枝番 INTEGER NOT NULL,
    顧客区分 INTEGER DEFAULT 0,
    請求先コード VARCHAR(8) NOT NULL,
    請求先枝番 INTEGER NOT NULL,
    回収先コード VARCHAR(8) NOT NULL,
    回収先枝番 INTEGER NOT NULL,
    顧客名 VARCHAR(40),
    顧客名カナ VARCHAR(40),
    担当社員コード VARCHAR(10),
    顧客担当者名 VARCHAR(40),
    顧客担当部署名 VARCHAR(40),
    顧客郵便番号 CHAR(8),
    顧客都道府県 VARCHAR(4),
    顧客住所１ VARCHAR(40),
    顧客電話番号 VARCHAR(13),
    顧客メールアドレス VARCHAR(40),
    顧客締日１ INTEGER,
    顧客支払月数１ INTEGER,
    顧客支払日１ INTEGER,
    顧客支払方法１ INTEGER,
    顧客締日２ INTEGER,
    作成日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    作成者名 VARCHAR(12),
    更新日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    更新者名 VARCHAR(12),
    PRIMARY KEY (顧客コード, 顧客枝番),
    CONSTRAINT fk_customer_company FOREIGN KEY (顧客コード)
        REFERENCES 取引先マスタ(取引先コード) ON DELETE CASCADE
);

-- インデックス
CREATE INDEX idx_customer_company ON 顧客マスタ(顧客コード);

-- コメント
COMMENT ON TABLE 顧客マスタ IS '顧客としての詳細情報を管理するテーブル';
COMMENT ON COLUMN 顧客マスタ.顧客コード IS '取引先マスタへの外部キー';
COMMENT ON COLUMN 顧客マスタ.顧客枝番 IS '同一取引先内での枝番';
COMMENT ON COLUMN 顧客マスタ.顧客締日１ IS '締日(月の何日か)';
COMMENT ON COLUMN 顧客マスタ.顧客支払方法１ IS '支払方法(1:現金、2:振込、3:手形など)';
