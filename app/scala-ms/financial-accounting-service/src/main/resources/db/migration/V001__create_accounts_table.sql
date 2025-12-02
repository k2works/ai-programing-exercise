-- V001: 勘定科目テーブルの作成
-- 財務会計コンテキストの基本テーブル

CREATE TABLE IF NOT EXISTS accounts (
    account_id BIGSERIAL PRIMARY KEY,
    account_code VARCHAR(10) NOT NULL UNIQUE,
    account_name VARCHAR(100) NOT NULL,
    account_type VARCHAR(20) NOT NULL,
    is_summary_account BOOLEAN NOT NULL DEFAULT FALSE,
    display_order INTEGER NOT NULL DEFAULT 0,
    is_aggregation_target BOOLEAN NOT NULL DEFAULT TRUE,
    balance DECIMAL(15, 2) NOT NULL DEFAULT 0,
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);

-- インデックス
CREATE INDEX IF NOT EXISTS idx_accounts_account_type ON accounts(account_type);
CREATE INDEX IF NOT EXISTS idx_accounts_display_order ON accounts(display_order);

-- コメント
COMMENT ON TABLE accounts IS '勘定科目マスタ';
COMMENT ON COLUMN accounts.account_id IS '勘定科目ID';
COMMENT ON COLUMN accounts.account_code IS '勘定科目コード';
COMMENT ON COLUMN accounts.account_name IS '勘定科目名';
COMMENT ON COLUMN accounts.account_type IS '勘定科目種別（asset, liability, equity, revenue, expense）';
COMMENT ON COLUMN accounts.is_summary_account IS '合計科目フラグ';
COMMENT ON COLUMN accounts.display_order IS '表示順序';
COMMENT ON COLUMN accounts.is_aggregation_target IS '集計対象フラグ';
COMMENT ON COLUMN accounts.balance IS '残高';
COMMENT ON COLUMN accounts.created_at IS '作成日時';
COMMENT ON COLUMN accounts.updated_at IS '更新日時';
