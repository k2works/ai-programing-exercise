-- 仕入データ
-- 目的: 仕入（入荷）情報を管理するヘッダテーブル
-- 特徴: 発注データへの参照、仕入データ明細の親テーブル
CREATE TABLE 仕入データ (
    仕入番号 VARCHAR(10) PRIMARY KEY,
    仕入日 TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    仕入先コード VARCHAR(8) NOT NULL,
    仕入先枝番 INTEGER DEFAULT 0,
    社員コード VARCHAR(10) NOT NULL,
    開始日 TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    発注番号 VARCHAR(10),
    部門コード VARCHAR(6) NOT NULL,
    仕入金額合計 INTEGER DEFAULT 0,
    消費税合計 INTEGER DEFAULT 0,
    備考 VARCHAR(1000),
    作成日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    作成者名 VARCHAR(100),
    更新日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    更新者名 VARCHAR(100),

    CONSTRAINT fk_purchase_supplier FOREIGN KEY (仕入先コード, 仕入先枝番)
        REFERENCES 仕入先マスタ(仕入先コード, 仕入先枝番),
    CONSTRAINT fk_purchase_employee FOREIGN KEY (社員コード)
        REFERENCES 社員マスタ(社員コード),
    CONSTRAINT fk_purchase_po FOREIGN KEY (発注番号)
        REFERENCES 発注データ(発注番号),
    CONSTRAINT fk_purchase_department FOREIGN KEY (部門コード, 開始日)
        REFERENCES 部門マスタ(部門コード, 開始日)
);

COMMENT ON TABLE 仕入データ IS '仕入（入荷）情報を管理するヘッダ';
COMMENT ON COLUMN 仕入データ.仕入番号 IS '仕入を一意に識別する番号';
COMMENT ON COLUMN 仕入データ.仕入日 IS '仕入（入荷）した日付';
COMMENT ON COLUMN 仕入データ.仕入先コード IS '仕入先';
COMMENT ON COLUMN 仕入データ.仕入先枝番 IS '仕入先の枝番';
COMMENT ON COLUMN 仕入データ.社員コード IS '仕入担当者';
COMMENT ON COLUMN 仕入データ.発注番号 IS '元となる発注データへの参照（任意）';
COMMENT ON COLUMN 仕入データ.部門コード IS '仕入を計上する部門';

CREATE INDEX idx_purchase_supplier ON 仕入データ(仕入先コード, 仕入先枝番);
CREATE INDEX idx_purchase_employee ON 仕入データ(社員コード);
CREATE INDEX idx_purchase_po ON 仕入データ(発注番号);
CREATE INDEX idx_purchase_date ON 仕入データ(仕入日);
