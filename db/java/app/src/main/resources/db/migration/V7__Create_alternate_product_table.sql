-- 代替商品
-- 目的: 在庫切れ時などに提案できる代替商品を管理するため
-- 特徴: 商品マスタへの自己参照外部キー、優先順位で複数の代替候補を管理
CREATE TABLE 代替商品 (
    商品コード VARCHAR(20) NOT NULL,
    代替商品コード VARCHAR(20) NOT NULL,
    優先順位 INTEGER NOT NULL,
    作成日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    作成者名 VARCHAR(100) NOT NULL,
    更新日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    更新者名 VARCHAR(100) NOT NULL,

    PRIMARY KEY (商品コード, 代替商品コード),

    CONSTRAINT fk_alternate_source FOREIGN KEY (商品コード)
        REFERENCES 商品マスタ(商品コード),
    CONSTRAINT fk_alternate_target FOREIGN KEY (代替商品コード)
        REFERENCES 商品マスタ(商品コード)
);

COMMENT ON TABLE 代替商品 IS '在庫切れ時などに提案する代替商品';
COMMENT ON COLUMN 代替商品.商品コード IS '元商品コード';
COMMENT ON COLUMN 代替商品.代替商品コード IS '代替商品コード（商品マスタへの自己参照）';
COMMENT ON COLUMN 代替商品.優先順位 IS '代替提案の優先順位（数値が小さいほど優先）';
COMMENT ON COLUMN 代替商品.作成日時 IS 'レコード作成日時';
COMMENT ON COLUMN 代替商品.作成者名 IS 'レコード作成者のユーザー名';
COMMENT ON COLUMN 代替商品.更新日時 IS 'レコード更新日時';
COMMENT ON COLUMN 代替商品.更新者名 IS 'レコード更新者のユーザー名';

-- 優先順位での並び替え用インデックス
CREATE INDEX idx_alternate_priority ON 代替商品(商品コード, 優先順位);
