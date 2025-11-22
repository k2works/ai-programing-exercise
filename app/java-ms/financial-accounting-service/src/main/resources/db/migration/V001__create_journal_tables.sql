-- ==========================================
-- Journals and Journal Entries Tables
-- ==========================================

-- Journals table (ヘッダー)
CREATE TABLE IF NOT EXISTS journals (
    journal_id SERIAL PRIMARY KEY,
    journal_date DATE NOT NULL,
    description VARCHAR(1000),
    fiscal_year INTEGER NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL
);

CREATE INDEX idx_journals_date ON journals(journal_date);
CREATE INDEX idx_journals_fiscal_year ON journals(fiscal_year);

-- Journal entries table (明細)
CREATE TABLE IF NOT EXISTS journal_entries (
    journal_entry_id SERIAL PRIMARY KEY,
    journal_id INTEGER NOT NULL,
    account_code VARCHAR(10) NOT NULL,
    debit_amount NUMERIC(15,2) NOT NULL DEFAULT 0,
    credit_amount NUMERIC(15,2) NOT NULL DEFAULT 0,
    description VARCHAR(1000),
    FOREIGN KEY (journal_id) REFERENCES journals(journal_id) ON DELETE CASCADE
);

CREATE INDEX idx_journal_entries_journal_id ON journal_entries(journal_id);
CREATE INDEX idx_journal_entries_account_code ON journal_entries(account_code);

-- Constraints
ALTER TABLE journal_entries
  ADD CONSTRAINT check_debit_amount
  CHECK (debit_amount >= 0);

ALTER TABLE journal_entries
  ADD CONSTRAINT check_credit_amount
  CHECK (credit_amount >= 0);

-- Comments
COMMENT ON TABLE journals IS '仕訳ヘッダー';
COMMENT ON TABLE journal_entries IS '仕訳明細';
COMMENT ON COLUMN journals.journal_date IS '仕訳日付';
COMMENT ON COLUMN journals.fiscal_year IS '会計年度';
COMMENT ON COLUMN journal_entries.debit_amount IS '借方金額';
COMMENT ON COLUMN journal_entries.credit_amount IS '貸方金額';
