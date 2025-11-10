-- 入金データ
-- 目的: 顧客からの入金情報を管理
-- 特徴: 請求データとの消込処理
CREATE TABLE 入金データ (
    入金番号 VARCHAR(10) PRIMARY KEY,
    入金日 TIMESTAMP,
    部門コード VARCHAR(6) NOT NULL,
    開始日 TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    顧客コード VARCHAR(8) NOT NULL,
    顧客枝番 INTEGER DEFAULT 0,
    支払方法区分 INTEGER DEFAULT 1,
    入金口座コード VARCHAR(8),
    入金金額 INTEGER DEFAULT 0,
    消込金額 INTEGER DEFAULT 0,
    作成日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    作成者名 VARCHAR(100),
    更新日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    更新者名 VARCHAR(100),
    プログラム更新日時 TIMESTAMP,
    更新プログラム名 VARCHAR(50),

    CONSTRAINT fk_credit_customer FOREIGN KEY (顧客コード, 顧客枝番)
        REFERENCES 顧客マスタ(顧客コード, 顧客枝番),
    CONSTRAINT fk_credit_department FOREIGN KEY (部門コード, 開始日)
        REFERENCES 部門マスタ(部門コード, 開始日),
    CONSTRAINT fk_credit_bank_account FOREIGN KEY (入金口座コード)
        REFERENCES 入金口座マスタ(入金口座コード)
);

COMMENT ON TABLE 入金データ IS '顧客からの入金情報を管理';
COMMENT ON COLUMN 入金データ.入金番号 IS '入金を一意に識別する番号';
COMMENT ON COLUMN 入金データ.入金日 IS '入金があった日付';
COMMENT ON COLUMN 入金データ.部門コード IS '入金を計上する部門';
COMMENT ON COLUMN 入金データ.顧客コード IS '入金元の顧客';
COMMENT ON COLUMN 入金データ.顧客枝番 IS '顧客の枝番';
COMMENT ON COLUMN 入金データ.支払方法区分 IS '入金方法（1=振込、2=現金など）';
COMMENT ON COLUMN 入金データ.入金口座コード IS '入金先の口座';
COMMENT ON COLUMN 入金データ.入金金額 IS '入金された金額';
COMMENT ON COLUMN 入金データ.消込金額 IS '請求に対して消込済みの金額';

CREATE INDEX idx_credit_customer ON 入金データ(顧客コード, 顧客枝番);
CREATE INDEX idx_credit_date ON 入金データ(入金日);
CREATE INDEX idx_credit_bank_account ON 入金データ(入金口座コード);
