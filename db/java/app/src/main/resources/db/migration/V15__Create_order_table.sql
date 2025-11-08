-- 受注データ
-- 目的: 顧客からの注文情報を管理するヘッダテーブル
-- 特徴: 受注データ明細の親テーブル、顧客・部門・社員への外部キー
CREATE TABLE 受注データ (
    受注番号 VARCHAR(10) PRIMARY KEY,
    受注日 TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    部門コード VARCHAR(6) NOT NULL,
    開始日 TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    顧客コード VARCHAR(8) NOT NULL,
    顧客枝番 INTEGER,
    社員コード VARCHAR(10) NOT NULL,
    希望納期 TIMESTAMP,
    客先注文番号 VARCHAR(20),
    倉庫コード VARCHAR(3) NOT NULL,
    受注金額合計 INTEGER DEFAULT 0,
    消費税合計 INTEGER DEFAULT 0,
    備考 VARCHAR(1000),
    作成日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    作成者名 VARCHAR(100),
    更新日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    更新者名 VARCHAR(100),

    CONSTRAINT fk_order_customer FOREIGN KEY (顧客コード, 顧客枝番)
        REFERENCES 顧客マスタ(顧客コード, 顧客枝番),
    CONSTRAINT fk_order_employee FOREIGN KEY (社員コード)
        REFERENCES 社員マスタ(社員コード),
    CONSTRAINT fk_order_department FOREIGN KEY (部門コード, 開始日)
        REFERENCES 部門マスタ(部門コード, 開始日)
);

COMMENT ON TABLE 受注データ IS '顧客からの注文情報を管理するヘッダ';
COMMENT ON COLUMN 受注データ.受注番号 IS '受注を一意に識別する番号';
COMMENT ON COLUMN 受注データ.受注日 IS '受注を受けた日付';
COMMENT ON COLUMN 受注データ.部門コード IS '受注を担当する部門';
COMMENT ON COLUMN 受注データ.顧客コード IS '注文した顧客';
COMMENT ON COLUMN 受注データ.顧客枝番 IS '顧客の枝番';
COMMENT ON COLUMN 受注データ.社員コード IS '受注担当者';
COMMENT ON COLUMN 受注データ.希望納期 IS '顧客が希望する納期';
COMMENT ON COLUMN 受注データ.客先注文番号 IS '顧客側の注文管理番号';
COMMENT ON COLUMN 受注データ.倉庫コード IS '出荷元倉庫';
COMMENT ON COLUMN 受注データ.受注金額合計 IS '受注明細の金額合計';
COMMENT ON COLUMN 受注データ.消費税合計 IS '消費税の合計額';

CREATE INDEX idx_order_customer ON 受注データ(顧客コード, 顧客枝番);
CREATE INDEX idx_order_employee ON 受注データ(社員コード);
CREATE INDEX idx_order_date ON 受注データ(受注日);
