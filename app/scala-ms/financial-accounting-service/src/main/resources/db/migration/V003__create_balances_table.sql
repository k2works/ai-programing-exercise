-- V003: 勘定科目残高テーブルの作成
-- 会計年度ごとの勘定科目残高を管理

CREATE TABLE IF NOT EXISTS account_balances (
    balance_id BIGSERIAL PRIMARY KEY,
    account_code VARCHAR(10) NOT NULL REFERENCES accounts(account_code),
    fiscal_year INTEGER NOT NULL,
    debit_total DECIMAL(15, 2) NOT NULL DEFAULT 0,
    credit_total DECIMAL(15, 2) NOT NULL DEFAULT 0,
    balance DECIMAL(15, 2) NOT NULL DEFAULT 0,
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    UNIQUE (account_code, fiscal_year)
);

-- インデックス
CREATE INDEX IF NOT EXISTS idx_account_balances_fiscal_year ON account_balances(fiscal_year);
CREATE INDEX IF NOT EXISTS idx_account_balances_account_code ON account_balances(account_code);

-- コメント
COMMENT ON TABLE account_balances IS '勘定科目残高';
COMMENT ON COLUMN account_balances.balance_id IS '残高ID';
COMMENT ON COLUMN account_balances.account_code IS '勘定科目コード';
COMMENT ON COLUMN account_balances.fiscal_year IS '会計年度';
COMMENT ON COLUMN account_balances.debit_total IS '借方合計';
COMMENT ON COLUMN account_balances.credit_total IS '貸方合計';
COMMENT ON COLUMN account_balances.balance IS '残高';
COMMENT ON COLUMN account_balances.created_at IS '作成日時';
COMMENT ON COLUMN account_balances.updated_at IS '更新日時';
