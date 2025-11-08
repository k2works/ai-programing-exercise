-- 顧客別販売単価
-- 目的: 特定の顧客に対して標準価格と異なる価格を設定するため
-- 特徴: 商品コード+取引先コードの複合主キー
CREATE TABLE 顧客別販売単価 (
    商品コード VARCHAR(20) NOT NULL,
    取引先コード VARCHAR(20) NOT NULL,
    販売単価 INTEGER NOT NULL,
    作成日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    作成者名 VARCHAR(100) NOT NULL,
    更新日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    更新者名 VARCHAR(100) NOT NULL,

    PRIMARY KEY (商品コード, 取引先コード),

    CONSTRAINT fk_price_product FOREIGN KEY (商品コード)
        REFERENCES 商品マスタ(商品コード)
);

COMMENT ON TABLE 顧客別販売単価 IS '特定顧客への個別価格設定';
COMMENT ON COLUMN 顧客別販売単価.商品コード IS '対象商品コード';
COMMENT ON COLUMN 顧客別販売単価.取引先コード IS '対象取引先コード';
COMMENT ON COLUMN 顧客別販売単価.販売単価 IS '顧客別の販売単価';
COMMENT ON COLUMN 顧客別販売単価.作成日時 IS 'レコード作成日時';
COMMENT ON COLUMN 顧客別販売単価.作成者名 IS 'レコード作成者のユーザー名';
COMMENT ON COLUMN 顧客別販売単価.更新日時 IS 'レコード更新日時';
COMMENT ON COLUMN 顧客別販売単価.更新者名 IS 'レコード更新者のユーザー名';

-- 取引先コードでの検索用インデックス
CREATE INDEX idx_price_customer ON 顧客別販売単価(取引先コード);
