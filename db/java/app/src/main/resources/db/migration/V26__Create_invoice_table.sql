-- 請求データ
-- 目的: 顧客への請求情報を管理するヘッダテーブル
-- 特徴: 請求データ明細の親テーブル、売上からの請求生成
CREATE TABLE 請求データ (
    請求番号 VARCHAR(10) PRIMARY KEY,
    請求日 TIMESTAMP,
    取引先コード VARCHAR(8) NOT NULL,
    顧客枝番 INTEGER DEFAULT 0,
    前回入金額 INTEGER DEFAULT 0,
    当月売上額 INTEGER DEFAULT 0,
    当月入金額 INTEGER DEFAULT 0,
    当月請求額 INTEGER DEFAULT 0,
    消費税金額 INTEGER DEFAULT 0,
    請求消込金額 INTEGER DEFAULT 0,
    作成日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    作成者名 VARCHAR(100),
    更新日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    更新者名 VARCHAR(100),

    CONSTRAINT fk_invoice_customer FOREIGN KEY (取引先コード)
        REFERENCES 取引先マスタ(取引先コード)
);

COMMENT ON TABLE 請求データ IS '顧客への請求情報を管理するヘッダ';
COMMENT ON COLUMN 請求データ.請求番号 IS '請求を一意に識別する番号';
COMMENT ON COLUMN 請求データ.請求日 IS '請求書を発行した日付';
COMMENT ON COLUMN 請求データ.取引先コード IS '請求先の取引先';
COMMENT ON COLUMN 請求データ.顧客枝番 IS '顧客の枝番';
COMMENT ON COLUMN 請求データ.前回入金額 IS '前回請求時点での入金済み額';
COMMENT ON COLUMN 請求データ.当月売上額 IS '当月の売上合計額';
COMMENT ON COLUMN 請求データ.当月入金額 IS '当月の入金合計額';
COMMENT ON COLUMN 請求データ.当月請求額 IS '当月の請求額（前回繰越+当月売上-当月入金）';
COMMENT ON COLUMN 請求データ.消費税金額 IS '消費税の合計額';
COMMENT ON COLUMN 請求データ.請求消込金額 IS '請求に対して消込済みの金額';

CREATE INDEX idx_invoice_company ON 請求データ(取引先コード);
CREATE INDEX idx_invoice_date ON 請求データ(請求日);
