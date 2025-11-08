-- 売上データ
-- 目的: 売上（出荷）情報を管理するヘッダテーブル
-- 特徴: 受注データへの参照、売上データ明細の親テーブル
CREATE TABLE 売上データ (
    売上番号 VARCHAR(10) PRIMARY KEY,
    受注番号 VARCHAR(10) NOT NULL,
    売上日 TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    売上区分 INTEGER DEFAULT 1,
    部門コード VARCHAR(6) NOT NULL,
    開始日 TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    取引先コード VARCHAR(8) NOT NULL,
    社員コード VARCHAR(10) NOT NULL,
    売上金額合計 INTEGER DEFAULT 0,
    消費税合計 INTEGER DEFAULT 0,
    備考 VARCHAR(1000),
    赤黒伝票番号 INTEGER,
    元伝票番号 VARCHAR(10),
    作成日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    作成者名 VARCHAR(100),
    更新日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    更新者名 VARCHAR(100),

    CONSTRAINT fk_sales_order FOREIGN KEY (受注番号)
        REFERENCES 受注データ(受注番号),
    CONSTRAINT fk_sales_company FOREIGN KEY (取引先コード)
        REFERENCES 取引先マスタ(取引先コード),
    CONSTRAINT fk_sales_employee FOREIGN KEY (社員コード)
        REFERENCES 社員マスタ(社員コード),
    CONSTRAINT fk_sales_department FOREIGN KEY (部門コード, 開始日)
        REFERENCES 部門マスタ(部門コード, 開始日)
);

COMMENT ON TABLE 売上データ IS '売上（出荷）情報を管理するヘッダ';
COMMENT ON COLUMN 売上データ.売上番号 IS '売上を一意に識別する番号';
COMMENT ON COLUMN 売上データ.受注番号 IS '元となる受注データへの参照';
COMMENT ON COLUMN 売上データ.売上日 IS '売上計上日';
COMMENT ON COLUMN 売上データ.売上区分 IS '売上の種類（1=通常売上、2=返品など）';
COMMENT ON COLUMN 売上データ.部門コード IS '売上を計上する部門';
COMMENT ON COLUMN 売上データ.取引先コード IS '売上先の取引先';
COMMENT ON COLUMN 売上データ.社員コード IS '売上担当者';
COMMENT ON COLUMN 売上データ.赤黒伝票番号 IS '返品・訂正時の赤黒伝票管理番号';
COMMENT ON COLUMN 売上データ.元伝票番号 IS '訂正元の売上伝票番号';

CREATE INDEX idx_sales_order ON 売上データ(受注番号);
CREATE INDEX idx_sales_company ON 売上データ(取引先コード);
CREATE INDEX idx_sales_date ON 売上データ(売上日);
