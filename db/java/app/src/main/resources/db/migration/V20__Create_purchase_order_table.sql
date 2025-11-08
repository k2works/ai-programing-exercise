-- 発注データ
-- 目的: 仕入先への発注情報を管理するヘッダテーブル
-- 特徴: 発注データ明細の親テーブル、受注との連携
CREATE TABLE 発注データ (
    発注番号 VARCHAR(10) PRIMARY KEY,
    発注日 TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    受注番号 VARCHAR(10) NOT NULL,
    仕入先コード VARCHAR(8) NOT NULL,
    仕入先枝番 INTEGER DEFAULT 0,
    社員コード VARCHAR(10) NOT NULL,
    指定納期 TIMESTAMP,
    倉庫コード VARCHAR(3) NOT NULL,
    発注金額合計 INTEGER DEFAULT 0,
    消費税合計 INTEGER DEFAULT 0,
    備考 VARCHAR(1000),
    作成日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    作成者名 VARCHAR(100),
    更新日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    更新者名 VARCHAR(100),

    CONSTRAINT fk_po_supplier FOREIGN KEY (仕入先コード, 仕入先枝番)
        REFERENCES 仕入先マスタ(仕入先コード, 仕入先枝番),
    CONSTRAINT fk_po_employee FOREIGN KEY (社員コード)
        REFERENCES 社員マスタ(社員コード),
    CONSTRAINT fk_po_order FOREIGN KEY (受注番号)
        REFERENCES 受注データ(受注番号),
    CONSTRAINT fk_po_warehouse FOREIGN KEY (倉庫コード)
        REFERENCES 倉庫マスタ(倉庫コード)
);

COMMENT ON TABLE 発注データ IS '仕入先への発注情報を管理するヘッダ';
COMMENT ON COLUMN 発注データ.発注番号 IS '発注を一意に識別する番号';
COMMENT ON COLUMN 発注データ.発注日 IS '発注を行った日付';
COMMENT ON COLUMN 発注データ.受注番号 IS '元となる受注データへの参照';
COMMENT ON COLUMN 発注データ.仕入先コード IS '発注先の仕入先';
COMMENT ON COLUMN 発注データ.仕入先枝番 IS '仕入先の枝番';
COMMENT ON COLUMN 発注データ.社員コード IS '発注担当者';
COMMENT ON COLUMN 発注データ.指定納期 IS '仕入先に指定した納期';
COMMENT ON COLUMN 発注データ.倉庫コード IS '納品先倉庫';

CREATE INDEX idx_po_supplier ON 発注データ(仕入先コード, 仕入先枝番);
CREATE INDEX idx_po_employee ON 発注データ(社員コード);
CREATE INDEX idx_po_order ON 発注データ(受注番号);
CREATE INDEX idx_po_date ON 発注データ(発注日);
