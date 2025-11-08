-- 取引先分類所属マスタ
-- 目的: 取引先と分類を紐づける（多対多の関係）
-- 特徴: 3つのカラムで複合主キーを構成
CREATE TABLE 取引先分類所属マスタ (
    取引先分類種別コード VARCHAR(2) NOT NULL,
    取引先分類コード VARCHAR(8) NOT NULL,
    取引先コード VARCHAR(8) NOT NULL,
    作成日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    作成者名 VARCHAR(100),
    更新日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    更新者名 VARCHAR(100),

    PRIMARY KEY (取引先分類種別コード, 取引先分類コード, 取引先コード),

    CONSTRAINT fk_category_group_category FOREIGN KEY (取引先分類種別コード, 取引先分類コード)
        REFERENCES 取引先分類マスタ(取引先分類種別コード, 取引先分類コード),
    CONSTRAINT fk_category_group_company FOREIGN KEY (取引先コード)
        REFERENCES 取引先マスタ(取引先コード)
);

COMMENT ON TABLE 取引先分類所属マスタ IS '取引先と分類の多対多関係を管理';
COMMENT ON COLUMN 取引先分類所属マスタ.取引先分類種別コード IS '分類種別コード';
COMMENT ON COLUMN 取引先分類所属マスタ.取引先分類コード IS '分類コード';
COMMENT ON COLUMN 取引先分類所属マスタ.取引先コード IS '取引先コード';

CREATE INDEX idx_category_group_company ON 取引先分類所属マスタ(取引先コード);
CREATE INDEX idx_category_group_category ON 取引先分類所属マスタ(取引先分類種別コード, 取引先分類コード);
