-- 社員マスタ
CREATE TABLE 社員マスタ (
    社員コード VARCHAR(10) PRIMARY KEY,
    社員名 VARCHAR(20),
    社員名カナ VARCHAR(40),
    パスワード VARCHAR(8),
    電話番号 VARCHAR(13),
    FAX番号 VARCHAR(13),
    部門コード VARCHAR(6) NOT NULL,
    開始日 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    職種コード VARCHAR(2),
    承認権限コード VARCHAR(2),
    作成日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    作成者名 VARCHAR(12),
    更新日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    更新者名 VARCHAR(12),
    CONSTRAINT fk_employee_department
        FOREIGN KEY (部門コード)
        REFERENCES 部門マスタ(部門コード)
        ON DELETE RESTRICT
        ON UPDATE CASCADE
);

-- インデックス作成
CREATE INDEX idx_employee_dept ON 社員マスタ(部門コード);

-- コメント
COMMENT ON TABLE 社員マスタ IS '社員情報を管理する社員マスタ';
COMMENT ON COLUMN 社員マスタ.社員コード IS '社員を一意に識別するコード';
COMMENT ON COLUMN 社員マスタ.社員名 IS '社員名';
COMMENT ON COLUMN 社員マスタ.部門コード IS '所属部門のコード';
COMMENT ON COLUMN 社員マスタ.開始日 IS '社員の勤務開始日または記録開始日';
