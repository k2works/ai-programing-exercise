-- 顧客別販売単価テーブル作成
CREATE TABLE IF NOT EXISTS 顧客別販売単価 (
    商品コード VARCHAR(16) NOT NULL,
    取引先コード VARCHAR(8) NOT NULL,
    販売単価 INTEGER NOT NULL DEFAULT 0,
    作成日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    作成者名 VARCHAR(12) NOT NULL,
    更新日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    更新者名 VARCHAR(12) NOT NULL,
    PRIMARY KEY (商品コード, 取引先コード),
    CONSTRAINT fk_price_product FOREIGN KEY (商品コード)
        REFERENCES 商品マスタ(商品コード) ON DELETE CASCADE
);

-- インデックス作成
CREATE INDEX IF NOT EXISTS idx_price_by_customer_comp ON 顧客別販売単価(取引先コード);

-- コメント追加
COMMENT ON TABLE 顧客別販売単価 IS '顧客別の販売単価を管理するテーブル';
COMMENT ON COLUMN 顧客別販売単価.商品コード IS '商品コード（外部キー）';
COMMENT ON COLUMN 顧客別販売単価.取引先コード IS '取引先コード';
COMMENT ON COLUMN 顧客別販売単価.販売単価 IS '顧客別販売単価';
