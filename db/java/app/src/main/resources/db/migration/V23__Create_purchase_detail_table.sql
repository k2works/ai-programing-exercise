-- 仕入データ明細
-- 目的: 仕入の商品別明細情報を管理
-- 特徴: 仕入番号+行番号の複合主キー
CREATE TABLE 仕入データ明細 (
    仕入番号 VARCHAR(10) NOT NULL,
    仕入行番号 INTEGER NOT NULL,
    商品コード VARCHAR(20) NOT NULL,
    商品名 VARCHAR(200) NOT NULL,
    仕入単価 INTEGER DEFAULT 0,
    仕入数量 INTEGER DEFAULT 1,
    ロット番号 VARCHAR(20),
    倉庫コード VARCHAR(3) NOT NULL,
    作成日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    作成者名 VARCHAR(100),
    更新日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    更新者名 VARCHAR(100),

    PRIMARY KEY (仕入番号, 仕入行番号),

    CONSTRAINT fk_purchase_detail_purchase FOREIGN KEY (仕入番号)
        REFERENCES 仕入データ(仕入番号) ON DELETE CASCADE,
    CONSTRAINT fk_purchase_detail_product FOREIGN KEY (商品コード)
        REFERENCES 商品マスタ(商品コード),
    CONSTRAINT fk_purchase_detail_warehouse FOREIGN KEY (倉庫コード)
        REFERENCES 倉庫マスタ(倉庫コード)
);

COMMENT ON TABLE 仕入データ明細 IS '仕入の商品別明細情報';
COMMENT ON COLUMN 仕入データ明細.仕入番号 IS '親の仕入データへの外部キー';
COMMENT ON COLUMN 仕入データ明細.仕入行番号 IS '仕入内での明細行番号';
COMMENT ON COLUMN 仕入データ明細.商品コード IS '仕入した商品';
COMMENT ON COLUMN 仕入データ明細.商品名 IS '仕入時の商品名（履歴保持）';
COMMENT ON COLUMN 仕入データ明細.仕入単価 IS '仕入時の単価（履歴保持）';
COMMENT ON COLUMN 仕入データ明細.仕入数量 IS '仕入した数量';
COMMENT ON COLUMN 仕入データ明細.ロット番号 IS '商品のロット管理番号';
COMMENT ON COLUMN 仕入データ明細.倉庫コード IS '入荷先倉庫';

CREATE INDEX idx_purchase_detail_product ON 仕入データ明細(商品コード);
CREATE INDEX idx_purchase_detail_warehouse ON 仕入データ明細(倉庫コード);
