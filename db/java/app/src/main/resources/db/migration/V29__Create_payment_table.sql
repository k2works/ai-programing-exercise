-- 支払データ
-- 目的: 仕入先への支払情報を管理
-- 特徴: 仕入データからの支払生成
CREATE TABLE 支払データ (
    支払番号 VARCHAR(10) PRIMARY KEY,
    支払日 INTEGER,
    部門コード VARCHAR(6) NOT NULL,
    部門開始日 TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    仕入先コード VARCHAR(8) NOT NULL,
    仕入先枝番 INTEGER DEFAULT 0,
    支払方法区分 INTEGER DEFAULT 1,
    支払金額 INTEGER DEFAULT 0,
    消費税合計 INTEGER DEFAULT 0,
    支払完了フラグ INTEGER DEFAULT 0,
    作成日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    作成者名 VARCHAR(100),
    更新日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    更新者名 VARCHAR(100),

    CONSTRAINT fk_payment_supplier FOREIGN KEY (仕入先コード, 仕入先枝番)
        REFERENCES 仕入先マスタ(仕入先コード, 仕入先枝番),
    CONSTRAINT fk_payment_department FOREIGN KEY (部門コード, 部門開始日)
        REFERENCES 部門マスタ(部門コード, 開始日)
);

COMMENT ON TABLE 支払データ IS '仕入先への支払情報を管理';
COMMENT ON COLUMN 支払データ.支払番号 IS '支払を一意に識別する番号';
COMMENT ON COLUMN 支払データ.支払日 IS '支払を行う日（YYYYMMDD形式の数値）';
COMMENT ON COLUMN 支払データ.部門コード IS '支払を計上する部門';
COMMENT ON COLUMN 支払データ.仕入先コード IS '支払先の仕入先';
COMMENT ON COLUMN 支払データ.仕入先枝番 IS '仕入先の枝番';
COMMENT ON COLUMN 支払データ.支払方法区分 IS '支払方法（1=振込、2=手形など）';
COMMENT ON COLUMN 支払データ.支払金額 IS '支払金額';
COMMENT ON COLUMN 支払データ.消費税合計 IS '消費税の合計額';
COMMENT ON COLUMN 支払データ.支払完了フラグ IS '支払完了状態（0=未完了、1=完了）';

CREATE INDEX idx_payment_supplier ON 支払データ(仕入先コード, 仕入先枝番);
CREATE INDEX idx_payment_date ON 支払データ(支払日);
