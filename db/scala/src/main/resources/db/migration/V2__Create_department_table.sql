-- 部門マスタテーブル
CREATE TABLE IF NOT EXISTS 部門マスタ (
    部門コード VARCHAR(6) PRIMARY KEY,
    開始日 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    終了日 TIMESTAMP DEFAULT '2100-12-31 00:00:00',
    部門名 VARCHAR(40),
    組織階層 INT DEFAULT 0,
    部門パス VARCHAR(100) NOT NULL,
    最下層区分 INT DEFAULT 0,
    伝票入力可否 INT DEFAULT 0,
    作成日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    作成者名 VARCHAR(50) NOT NULL,
    更新日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    更新者名 VARCHAR(50) NOT NULL,
    UNIQUE (部門コード, 開始日)
);

-- インデックス
CREATE INDEX IF NOT EXISTS idx_department_name ON 部門マスタ(部門名);
CREATE INDEX IF NOT EXISTS idx_department_path ON 部門マスタ(部門パス);

-- コメント(PostgreSQL用)
COMMENT ON TABLE 部門マスタ IS '部門マスタ';
COMMENT ON COLUMN 部門マスタ.部門コード IS '部門コード';
COMMENT ON COLUMN 部門マスタ.開始日 IS '開始日';
COMMENT ON COLUMN 部門マスタ.終了日 IS '終了日';
COMMENT ON COLUMN 部門マスタ.部門名 IS '部門名';
COMMENT ON COLUMN 部門マスタ.組織階層 IS '組織階層';
COMMENT ON COLUMN 部門マスタ.部門パス IS '部門パス';
COMMENT ON COLUMN 部門マスタ.最下層区分 IS '最下層区分';
COMMENT ON COLUMN 部門マスタ.伝票入力可否 IS '伝票入力可否';
COMMENT ON COLUMN 部門マスタ.作成日時 IS '作成日時';
COMMENT ON COLUMN 部門マスタ.作成者名 IS '作成者名';
COMMENT ON COLUMN 部門マスタ.更新日時 IS '更新日時';
COMMENT ON COLUMN 部門マスタ.更新者名 IS '更新者名';
