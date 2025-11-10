-- 与信残高データ
-- 目的: 取引先ごとの与信残高を管理する
-- 特徴: 受注残高、債権残高、債務残高を追跡

CREATE TABLE 与信残高データ (
    取引先コード VARCHAR(8) PRIMARY KEY,
    受注残高 INTEGER DEFAULT 0,
    債権残高 INTEGER DEFAULT 0,
    債務残高 INTEGER DEFAULT 0,
    作成日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    作成者名 VARCHAR(100),
    更新日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    更新者名 VARCHAR(100),

    CONSTRAINT fk_credit_balance_company FOREIGN KEY (取引先コード)
        REFERENCES 取引先マスタ(取引先コード)
);

COMMENT ON TABLE 与信残高データ IS '取引先ごとの与信残高を管理';
COMMENT ON COLUMN 与信残高データ.取引先コード IS '取引先を一意に識別するコード';
COMMENT ON COLUMN 与信残高データ.受注残高 IS '受注の残高（受注済み未出荷分）';
COMMENT ON COLUMN 与信残高データ.債権残高 IS '売上債権の残高（売上済み未回収分）';
COMMENT ON COLUMN 与信残高データ.債務残高 IS '仕入債務の残高（仕入済み未支払分）';

CREATE INDEX idx_credit_balance_order ON 与信残高データ(受注残高);
CREATE INDEX idx_credit_balance_receivable ON 与信残高データ(債権残高);
