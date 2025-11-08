-- 社員マスタ
CREATE TABLE 社員マスタ (
    社員コード VARCHAR(20) NOT NULL,
    社員名 VARCHAR(100) NOT NULL,
    社員名カナ VARCHAR(100),
    性別 VARCHAR(1) CHECK (性別 IN ('M', 'F', 'O')),
    生年月日 DATE,
    入社年月日 DATE NOT NULL,
    部門コード VARCHAR(20) NOT NULL,
    役職コード VARCHAR(20),

    作成日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    作成者名 VARCHAR(50) NOT NULL,
    更新日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    更新者名 VARCHAR(50) NOT NULL,

    CONSTRAINT pk_社員マスタ PRIMARY KEY (社員コード)
);

COMMENT ON TABLE 社員マスタ IS '社員の基本情報を管理するマスタ';
COMMENT ON COLUMN 社員マスタ.社員コード IS '社員の一意識別子';
COMMENT ON COLUMN 社員マスタ.社員名 IS '社員の氏名';
COMMENT ON COLUMN 社員マスタ.社員名カナ IS '社員の氏名（カナ）';
COMMENT ON COLUMN 社員マスタ.性別 IS '性別（M:男性, F:女性, O:その他）';
COMMENT ON COLUMN 社員マスタ.生年月日 IS '生年月日';
COMMENT ON COLUMN 社員マスタ.入社年月日 IS '入社年月日';
COMMENT ON COLUMN 社員マスタ.部門コード IS '所属部門コード';
COMMENT ON COLUMN 社員マスタ.役職コード IS '役職コード';

-- インデックス
CREATE INDEX idx_社員マスタ_部門コード ON 社員マスタ(部門コード);
CREATE INDEX idx_社員マスタ_社員名カナ ON 社員マスタ(社員名カナ);
