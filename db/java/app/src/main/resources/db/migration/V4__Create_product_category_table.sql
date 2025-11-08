-- 商品分類マスタ
-- 目的: 商品を階層的に分類し、効率的に検索・集計するため
-- 特徴: 商品分類階層・商品分類パスで階層構造を表現
CREATE TABLE 商品分類マスタ (
    商品分類コード VARCHAR(20) PRIMARY KEY,
    商品分類名 VARCHAR(100) NOT NULL,
    商品分類階層 INTEGER NOT NULL,
    商品分類パス TEXT NOT NULL,
    最下層区分 INTEGER NOT NULL,
    作成日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    作成者名 VARCHAR(100) NOT NULL,
    更新日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    更新者名 VARCHAR(100) NOT NULL
);

COMMENT ON TABLE 商品分類マスタ IS '商品を階層的に分類するマスタ';
COMMENT ON COLUMN 商品分類マスタ.商品分類コード IS '商品分類を一意に識別するコード';
COMMENT ON COLUMN 商品分類マスタ.商品分類名 IS '商品分類の名称';
COMMENT ON COLUMN 商品分類マスタ.商品分類階層 IS '階層レベル（1=第1階層、2=第2階層...）';
COMMENT ON COLUMN 商品分類マスタ.商品分類パス IS '階層パス（例：CAT001/CAT00101/CAT0010101）';
COMMENT ON COLUMN 商品分類マスタ.最下層区分 IS '最下層か否か（0=親階層あり、1=最下層）';
COMMENT ON COLUMN 商品分類マスタ.作成日時 IS 'レコード作成日時';
COMMENT ON COLUMN 商品分類マスタ.作成者名 IS 'レコード作成者のユーザー名';
COMMENT ON COLUMN 商品分類マスタ.更新日時 IS 'レコード更新日時';
COMMENT ON COLUMN 商品分類マスタ.更新者名 IS 'レコード更新者のユーザー名';

-- 階層レベルでのインデックス
CREATE INDEX idx_product_category_level ON 商品分類マスタ(商品分類階層);

-- パス検索用のインデックス
CREATE INDEX idx_product_category_path ON 商品分類マスタ(商品分類パス);
