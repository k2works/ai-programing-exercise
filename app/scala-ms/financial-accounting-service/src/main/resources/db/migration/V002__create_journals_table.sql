-- V002: 仕訳テーブルの作成
-- 複式簿記に基づく取引記録

CREATE TABLE IF NOT EXISTS journals (
    journal_id BIGSERIAL PRIMARY KEY,
    journal_date DATE NOT NULL,
    description VARCHAR(500) NOT NULL,
    fiscal_year INTEGER NOT NULL,
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);

-- 仕訳明細テーブル
CREATE TABLE IF NOT EXISTS journal_entries (
    entry_id BIGSERIAL PRIMARY KEY,
    journal_id BIGINT NOT NULL REFERENCES journals(journal_id) ON DELETE CASCADE,
    account_code VARCHAR(10) NOT NULL REFERENCES accounts(account_code),
    debit_amount DECIMAL(15, 2) NOT NULL DEFAULT 0,
    credit_amount DECIMAL(15, 2) NOT NULL DEFAULT 0,
    description VARCHAR(500) NOT NULL DEFAULT ''
);

-- インデックス
CREATE INDEX IF NOT EXISTS idx_journals_journal_date ON journals(journal_date);
CREATE INDEX IF NOT EXISTS idx_journals_fiscal_year ON journals(fiscal_year);
CREATE INDEX IF NOT EXISTS idx_journal_entries_journal_id ON journal_entries(journal_id);
CREATE INDEX IF NOT EXISTS idx_journal_entries_account_code ON journal_entries(account_code);

-- コメント
COMMENT ON TABLE journals IS '仕訳ヘッダー';
COMMENT ON COLUMN journals.journal_id IS '仕訳ID';
COMMENT ON COLUMN journals.journal_date IS '仕訳日付';
COMMENT ON COLUMN journals.description IS '摘要';
COMMENT ON COLUMN journals.fiscal_year IS '会計年度';
COMMENT ON COLUMN journals.created_at IS '作成日時';
COMMENT ON COLUMN journals.updated_at IS '更新日時';

COMMENT ON TABLE journal_entries IS '仕訳明細';
COMMENT ON COLUMN journal_entries.entry_id IS '仕訳明細ID';
COMMENT ON COLUMN journal_entries.journal_id IS '仕訳ID';
COMMENT ON COLUMN journal_entries.account_code IS '勘定科目コード';
COMMENT ON COLUMN journal_entries.debit_amount IS '借方金額';
COMMENT ON COLUMN journal_entries.credit_amount IS '貸方金額';
COMMENT ON COLUMN journal_entries.description IS '摘要';
