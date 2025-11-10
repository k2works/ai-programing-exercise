-- 自動採番マスタ
-- 目的: 伝票番号の自動採番を管理する
-- 特徴: 伝票種別と年月の複合主キー、排他制御による一意性保証

CREATE TABLE 自動採番マスタ (
    伝票種別コード VARCHAR(2) NOT NULL,
    年月 TIMESTAMP NOT NULL,
    最終伝票番号 INTEGER DEFAULT 0,

    PRIMARY KEY (伝票種別コード, 年月)
);

COMMENT ON TABLE 自動採番マスタ IS '伝票番号の自動採番を管理するマスタ';
COMMENT ON COLUMN 自動採番マスタ.伝票種別コード IS '伝票の種類を識別するコード（01=受注、02=売上など）';
COMMENT ON COLUMN 自動採番マスタ.年月 IS '採番の対象年月';
COMMENT ON COLUMN 自動採番マスタ.最終伝票番号 IS 'その月に発番された最終の伝票番号';

CREATE INDEX idx_auto_number_yearmonth ON 自動採番マスタ(年月);

-- 初期データ挿入（よく使う伝票種別）
INSERT INTO 自動採番マスタ (伝票種別コード, 年月, 最終伝票番号) VALUES
('01', CURRENT_TIMESTAMP, 0), -- 受注
('02', CURRENT_TIMESTAMP, 0), -- 売上
('03', CURRENT_TIMESTAMP, 0), -- 発注
('04', CURRENT_TIMESTAMP, 0), -- 仕入
('05', CURRENT_TIMESTAMP, 0), -- 請求
('06', CURRENT_TIMESTAMP, 0), -- 入金
('07', CURRENT_TIMESTAMP, 0); -- 支払
