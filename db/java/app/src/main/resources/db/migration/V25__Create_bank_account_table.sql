-- 入金口座マスタ
-- 目的: 入金先の銀行口座情報を管理するマスタテーブル
-- 特徴: 入金データの親テーブル
CREATE TABLE 入金口座マスタ (
    入金口座コード VARCHAR(8) PRIMARY KEY,
    口座名義 VARCHAR(100) NOT NULL,
    銀行名 VARCHAR(100),
    支店名 VARCHAR(100),
    口座種別 INTEGER DEFAULT 1,
    口座番号 VARCHAR(20),
    作成日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    作成者名 VARCHAR(100),
    更新日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    更新者名 VARCHAR(100)
);

COMMENT ON TABLE 入金口座マスタ IS '入金先の銀行口座情報を管理するマスタ';
COMMENT ON COLUMN 入金口座マスタ.入金口座コード IS '入金口座を一意に識別するコード';
COMMENT ON COLUMN 入金口座マスタ.口座名義 IS '口座の名義人';
COMMENT ON COLUMN 入金口座マスタ.銀行名 IS '銀行の名称';
COMMENT ON COLUMN 入金口座マスタ.支店名 IS '支店の名称';
COMMENT ON COLUMN 入金口座マスタ.口座種別 IS '口座の種類（1=普通、2=当座など）';
COMMENT ON COLUMN 入金口座マスタ.口座番号 IS '口座番号';
