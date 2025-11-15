-- 受注テーブル作成
CREATE TABLE IF NOT EXISTS 受注 (
    受注番号 VARCHAR(20) PRIMARY KEY,
    受注日 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    部門コード VARCHAR(6) NOT NULL,
    開始日 TIMESTAMP NOT NULL,
    顧客コード VARCHAR(8) NOT NULL,
    顧客枝番 INTEGER NOT NULL,
    社員コード VARCHAR(10) NOT NULL,
    納期 TIMESTAMP,
    顧客注文番号 VARCHAR(20),
    倉庫コード VARCHAR(3),
    受注金額 INTEGER DEFAULT 0,
    消費税 INTEGER DEFAULT 0,
    伝票コメント VARCHAR(1000),
    作成日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    作成者名 VARCHAR(12),
    更新日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    更新者名 VARCHAR(12),
    CONSTRAINT fk_order_customer FOREIGN KEY (顧客コード, 顧客枝番)
        REFERENCES 顧客マスタ(顧客コード, 顧客枝番),
    CONSTRAINT fk_order_employee FOREIGN KEY (社員コード)
        REFERENCES 社員マスタ(社員コード),
    CONSTRAINT fk_order_department FOREIGN KEY (部門コード, 開始日)
        REFERENCES 部門マスタ(部門コード, 開始日)
);

-- インデックス作成
CREATE INDEX IF NOT EXISTS idx_order_customer ON 受注(顧客コード, 顧客枝番);
CREATE INDEX IF NOT EXISTS idx_order_date ON 受注(受注日);
CREATE INDEX IF NOT EXISTS idx_order_employee ON 受注(社員コード);

-- コメント追加
COMMENT ON TABLE 受注 IS '顧客からの受注データを管理するテーブル';
COMMENT ON COLUMN 受注.受注番号 IS '受注を一意に識別する番号';
COMMENT ON COLUMN 受注.受注日 IS '受注を受け付けた日時';
COMMENT ON COLUMN 受注.納期 IS '顧客が希望する納期';
COMMENT ON COLUMN 受注.顧客注文番号 IS '顧客側の注文番号';
COMMENT ON COLUMN 受注.受注金額 IS '受注の合計金額（税抜）';
COMMENT ON COLUMN 受注.消費税 IS '消費税の合計額';
