-- 取引先分類マスタ
-- 目的: 取引先の具体的な分類を定義（IT業、製造業、関東など）
-- 特徴: 分類種別コード+分類コードの複合主キー
CREATE TABLE 取引先分類マスタ (
    取引先分類種別コード VARCHAR(2) NOT NULL,
    取引先分類コード VARCHAR(8) NOT NULL,
    取引先分類名 VARCHAR(30),
    作成日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    作成者名 VARCHAR(100),
    更新日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    更新者名 VARCHAR(100),

    PRIMARY KEY (取引先分類種別コード, 取引先分類コード),

    CONSTRAINT fk_company_category_type FOREIGN KEY (取引先分類種別コード)
        REFERENCES 取引先分類種別マスタ(取引先分類種別コード)
);

COMMENT ON TABLE 取引先分類マスタ IS '取引先の具体的な分類を定義';
COMMENT ON COLUMN 取引先分類マスタ.取引先分類種別コード IS '分類種別への外部キー';
COMMENT ON COLUMN 取引先分類マスタ.取引先分類コード IS '分類を一意に識別するコード';
COMMENT ON COLUMN 取引先分類マスタ.取引先分類名 IS '分類の名称';

CREATE INDEX idx_company_category_type ON 取引先分類マスタ(取引先分類種別コード);
