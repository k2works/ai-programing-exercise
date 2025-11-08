-- 在庫データ
-- 目的: 倉庫別・商品別・ロット別の在庫数量を管理
-- 特徴: 5つのフィールドからなる複合主キー、実在庫と有効在庫の区別
CREATE TABLE 在庫データ (
    倉庫コード VARCHAR(3) NOT NULL,
    商品コード VARCHAR(20) NOT NULL,
    ロット番号 VARCHAR(20) NOT NULL,
    在庫区分 VARCHAR(1) NOT NULL DEFAULT '1',
    良品区分 VARCHAR(1) NOT NULL DEFAULT 'G',
    実在庫数 INTEGER DEFAULT 0,
    有効在庫数 INTEGER DEFAULT 0,
    最終出荷日 TIMESTAMP,
    作成日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    作成者名 VARCHAR(100),
    更新日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    更新者名 VARCHAR(100),

    PRIMARY KEY (倉庫コード, 商品コード, ロット番号, 在庫区分, 良品区分),

    CONSTRAINT fk_stock_warehouse FOREIGN KEY (倉庫コード)
        REFERENCES 倉庫マスタ(倉庫コード),
    CONSTRAINT fk_stock_product FOREIGN KEY (商品コード)
        REFERENCES 商品マスタ(商品コード)
);

COMMENT ON TABLE 在庫データ IS '倉庫別・商品別・ロット別の在庫数量を管理';
COMMENT ON COLUMN 在庫データ.倉庫コード IS '在庫がある倉庫';
COMMENT ON COLUMN 在庫データ.商品コード IS '在庫商品';
COMMENT ON COLUMN 在庫データ.ロット番号 IS '商品のロット管理番号';
COMMENT ON COLUMN 在庫データ.在庫区分 IS '在庫の種類（1=通常在庫、2=預託在庫など）';
COMMENT ON COLUMN 在庫データ.良品区分 IS '品質区分（G=良品、B=不良品、H=保留品）';
COMMENT ON COLUMN 在庫データ.実在庫数 IS '物理的に存在する在庫数';
COMMENT ON COLUMN 在庫データ.有効在庫数 IS '販売可能な在庫数（実在庫 - 引当数量）';
COMMENT ON COLUMN 在庫データ.最終出荷日 IS '最後に出荷した日付';

CREATE INDEX idx_stock_warehouse ON 在庫データ(倉庫コード);
CREATE INDEX idx_stock_product ON 在庫データ(商品コード);
CREATE INDEX idx_stock_lot ON 在庫データ(ロット番号);
