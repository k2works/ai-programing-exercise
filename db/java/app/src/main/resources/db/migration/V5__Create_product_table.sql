-- 商品マスタ
-- 目的: 販売する商品の基本情報を管理するため
-- 特徴: 商品分類との外部キー、価格情報、在庫管理フラグなど業務に必要な属性を保持
CREATE TABLE 商品マスタ (
    商品コード VARCHAR(20) PRIMARY KEY,
    商品正式名 VARCHAR(200) NOT NULL,
    商品略称 VARCHAR(100),
    商品名カナ VARCHAR(200),
    商品区分 VARCHAR(10) NOT NULL,
    製品型番 VARCHAR(100),
    販売単価 INTEGER NOT NULL,
    仕入単価 INTEGER,
    売上原価 INTEGER,
    税区分 INTEGER NOT NULL,
    商品分類コード VARCHAR(20) NOT NULL,
    雑区分 INTEGER,
    在庫管理対象区分 INTEGER NOT NULL,
    在庫引当区分 INTEGER,
    仕入先コード VARCHAR(20),
    仕入先枝番 INTEGER,
    作成日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    作成者名 VARCHAR(100) NOT NULL,
    更新日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    更新者名 VARCHAR(100) NOT NULL,

    CONSTRAINT fk_product_category FOREIGN KEY (商品分類コード)
        REFERENCES 商品分類マスタ(商品分類コード)
);

COMMENT ON TABLE 商品マスタ IS '販売する商品の基本情報を管理するマスタ';
COMMENT ON COLUMN 商品マスタ.商品コード IS '商品を一意に識別するコード';
COMMENT ON COLUMN 商品マスタ.商品正式名 IS '商品の正式名称';
COMMENT ON COLUMN 商品マスタ.商品略称 IS '商品の略称';
COMMENT ON COLUMN 商品マスタ.商品名カナ IS '商品名のカナ表記';
COMMENT ON COLUMN 商品マスタ.商品区分 IS '商品の区分（製品/商品/サービスなど）';
COMMENT ON COLUMN 商品マスタ.製品型番 IS 'メーカーの製品型番';
COMMENT ON COLUMN 商品マスタ.販売単価 IS '標準販売単価';
COMMENT ON COLUMN 商品マスタ.仕入単価 IS '仕入単価';
COMMENT ON COLUMN 商品マスタ.売上原価 IS '売上原価';
COMMENT ON COLUMN 商品マスタ.税区分 IS '税区分（課税/非課税など）';
COMMENT ON COLUMN 商品マスタ.商品分類コード IS '商品分類への外部キー';
COMMENT ON COLUMN 商品マスタ.雑区分 IS '雑品区分';
COMMENT ON COLUMN 商品マスタ.在庫管理対象区分 IS '在庫管理するか否か（0=対象外、1=対象）';
COMMENT ON COLUMN 商品マスタ.在庫引当区分 IS '在庫引当するか否か（0=しない、1=する）';
COMMENT ON COLUMN 商品マスタ.仕入先コード IS 'デフォルト仕入先のコード';
COMMENT ON COLUMN 商品マスタ.仕入先枝番 IS 'デフォルト仕入先の枝番';
COMMENT ON COLUMN 商品マスタ.作成日時 IS 'レコード作成日時';
COMMENT ON COLUMN 商品マスタ.作成者名 IS 'レコード作成者のユーザー名';
COMMENT ON COLUMN 商品マスタ.更新日時 IS 'レコード更新日時';
COMMENT ON COLUMN 商品マスタ.更新者名 IS 'レコード更新者のユーザー名';

-- 商品分類での検索用インデックス
CREATE INDEX idx_product_category ON 商品マスタ(商品分類コード);

-- 商品名での検索用インデックス
CREATE INDEX idx_product_name ON 商品マスタ(商品正式名);
