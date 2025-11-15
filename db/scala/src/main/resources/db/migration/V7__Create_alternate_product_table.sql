-- 代替商品テーブル作成
CREATE TABLE IF NOT EXISTS 代替商品 (
    商品コード VARCHAR(16) NOT NULL,
    代替商品コード VARCHAR(16) NOT NULL,
    優先順位 INTEGER DEFAULT 1,
    作成日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    作成者名 VARCHAR(12) NOT NULL,
    更新日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    更新者名 VARCHAR(12) NOT NULL,
    PRIMARY KEY (商品コード, 代替商品コード),
    CONSTRAINT fk_alternate_product FOREIGN KEY (商品コード)
        REFERENCES 商品マスタ(商品コード) ON DELETE CASCADE,
    CONSTRAINT fk_alternate_product_alt FOREIGN KEY (代替商品コード)
        REFERENCES 商品マスタ(商品コード) ON DELETE CASCADE
);

-- インデックス作成
CREATE INDEX IF NOT EXISTS idx_alternate_product_alt ON 代替商品(代替商品コード);

-- コメント追加
COMMENT ON TABLE 代替商品 IS '商品の代替商品を管理するテーブル（多対多関係）';
COMMENT ON COLUMN 代替商品.商品コード IS '元商品コード（外部キー）';
COMMENT ON COLUMN 代替商品.代替商品コード IS '代替商品コード（外部キー）';
COMMENT ON COLUMN 代替商品.優先順位 IS '代替優先順位（1が最優先）';
