-- 部門マスタ
CREATE TABLE 部門マスタ (
    部門コード VARCHAR(20) NOT NULL,
    開始日 DATE NOT NULL,
    終了日 DATE NOT NULL,
    部門名 VARCHAR(100) NOT NULL,
    組織階層 INTEGER NOT NULL DEFAULT 1,
    部門パス VARCHAR(500) NOT NULL,
    最下層区分 SMALLINT NOT NULL DEFAULT 1 CHECK (最下層区分 IN (0, 1)),
    伝票入力可否 SMALLINT NOT NULL DEFAULT 0 CHECK (伝票入力可否 IN (0, 1)),

    作成日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    作成者名 VARCHAR(50) NOT NULL,
    更新日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    更新者名 VARCHAR(50) NOT NULL,

    CONSTRAINT pk_部門マスタ PRIMARY KEY (部門コード, 開始日)
);

COMMENT ON TABLE 部門マスタ IS '組織の部門情報を管理するマスタ';
COMMENT ON COLUMN 部門マスタ.部門コード IS '部門の一意識別子';
COMMENT ON COLUMN 部門マスタ.開始日 IS '部門の有効開始日（履歴管理）';
COMMENT ON COLUMN 部門マスタ.終了日 IS '部門の有効終了日';
COMMENT ON COLUMN 部門マスタ.部門名 IS '部門の名称';
COMMENT ON COLUMN 部門マスタ.組織階層 IS '組織内での階層レベル（1:最上位, 2:第2階層...）';
COMMENT ON COLUMN 部門マスタ.部門パス IS '階層パス（例: 10000/11000/11101）';
COMMENT ON COLUMN 部門マスタ.最下層区分 IS '最下層かどうか（0:中間階層, 1:最下層）';
COMMENT ON COLUMN 部門マスタ.伝票入力可否 IS '伝票入力が可能か（0:不可, 1:可）';

-- インデックス
CREATE INDEX idx_部門マスタ_組織階層 ON 部門マスタ(組織階層);
CREATE INDEX idx_部門マスタ_部門パス ON 部門マスタ(部門パス);
