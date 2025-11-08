-- 倉庫マスタ
-- 目的: 在庫管理の場所を定義するマスタテーブル
-- 特徴: 在庫データの親テーブル
CREATE TABLE 倉庫マスタ (
    倉庫コード VARCHAR(3) PRIMARY KEY,
    倉庫名 VARCHAR(100) NOT NULL,
    倉庫区分 INTEGER DEFAULT 1,
    住所 VARCHAR(200),
    電話番号 VARCHAR(20),
    責任者コード VARCHAR(10),
    作成日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    作成者名 VARCHAR(100),
    更新日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    更新者名 VARCHAR(100),

    CONSTRAINT fk_warehouse_employee FOREIGN KEY (責任者コード)
        REFERENCES 社員マスタ(社員コード)
);

COMMENT ON TABLE 倉庫マスタ IS '在庫管理の場所を定義するマスタ';
COMMENT ON COLUMN 倉庫マスタ.倉庫コード IS '倉庫を一意に識別するコード';
COMMENT ON COLUMN 倉庫マスタ.倉庫名 IS '倉庫の名称';
COMMENT ON COLUMN 倉庫マスタ.倉庫区分 IS '倉庫の種類（1=自社倉庫、2=委託倉庫など）';
COMMENT ON COLUMN 倉庫マスタ.責任者コード IS '倉庫の管理責任者';

CREATE INDEX idx_warehouse_employee ON 倉庫マスタ(責任者コード);
