-- ==========================================
-- Accounting Master and Transaction Tables
-- ==========================================

-- 勘定科目マスタ (Account Master)
CREATE TABLE IF NOT EXISTS accounts (
    account_code VARCHAR(10) PRIMARY KEY,
    account_name VARCHAR(100) NOT NULL,
    account_type VARCHAR(20) NOT NULL,
    bs_pl_classification VARCHAR(1) NOT NULL,  -- 'B': Balance Sheet, 'P': P/L
    transaction_element VARCHAR(1) NOT NULL,   -- 取引要素区分
    expense_category VARCHAR(20),              -- 費用区分
    display_order INTEGER NOT NULL,
    aggregation_target BOOLEAN DEFAULT TRUE,
    balance NUMERIC(15,2) DEFAULT 0.00,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL
);

CREATE INDEX idx_accounts_type ON accounts(account_type);
CREATE INDEX idx_accounts_bs_pl ON accounts(bs_pl_classification);

-- 仕訳エントリ (Journal Entries - Header level)
CREATE TABLE IF NOT EXISTS journal_headers (
    voucher_number VARCHAR(20) PRIMARY KEY,
    journal_date DATE NOT NULL,
    description VARCHAR(1000),
    total_amount NUMERIC(15,2) NOT NULL,
    reference_number VARCHAR(50),
    created_by VARCHAR(50) NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_by VARCHAR(50) NOT NULL,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL
);

CREATE INDEX idx_journal_headers_date ON journal_headers(journal_date);
CREATE INDEX idx_journal_headers_ref ON journal_headers(reference_number);

-- 仕訳明細 (Journal Entry Details)
CREATE TABLE IF NOT EXISTS journal_details (
    voucher_number VARCHAR(20) NOT NULL,
    line_number INTEGER NOT NULL,
    account_code VARCHAR(10) NOT NULL,
    debit_amount NUMERIC(15,2) NOT NULL DEFAULT 0,
    credit_amount NUMERIC(15,2) NOT NULL DEFAULT 0,
    description VARCHAR(1000),
    PRIMARY KEY (voucher_number, line_number),
    FOREIGN KEY (voucher_number) REFERENCES journal_headers(voucher_number) ON DELETE CASCADE,
    FOREIGN KEY (account_code) REFERENCES accounts(account_code)
);

CREATE INDEX idx_journal_details_account ON journal_details(account_code);

-- 日次勘定科目残高 (Daily Account Balance)
CREATE TABLE IF NOT EXISTS daily_account_balances (
    balance_date DATE NOT NULL,
    account_code VARCHAR(10) NOT NULL,
    sub_account_code VARCHAR(20) DEFAULT '',
    department_code VARCHAR(20) DEFAULT '',
    project_code VARCHAR(20) DEFAULT '',
    settlement_flag INTEGER DEFAULT 0,      -- 決算仕訳フラグ
    debit_amount NUMERIC(15,2) NOT NULL DEFAULT 0,
    credit_amount NUMERIC(15,2) NOT NULL DEFAULT 0,
    PRIMARY KEY (balance_date, account_code, sub_account_code, department_code, project_code, settlement_flag),
    FOREIGN KEY (account_code) REFERENCES accounts(account_code)
);

CREATE INDEX idx_daily_balances_date ON daily_account_balances(balance_date);
CREATE INDEX idx_daily_balances_account ON daily_account_balances(account_code);

-- Comments
COMMENT ON TABLE accounts IS '勘定科目マスタ';
COMMENT ON TABLE journal_headers IS '仕訳エントリ（ヘッダー）';
COMMENT ON TABLE journal_details IS '仕訳明細';
COMMENT ON TABLE daily_account_balances IS '日次勘定科目残高';

COMMENT ON COLUMN accounts.account_code IS '勘定科目コード';
COMMENT ON COLUMN accounts.account_name IS '勘定科目名';
COMMENT ON COLUMN accounts.account_type IS '勘定科目種別';
COMMENT ON COLUMN accounts.bs_pl_classification IS 'BSPL区分 (B:貸借対照表, P:損益計算書)';
COMMENT ON COLUMN accounts.transaction_element IS '取引要素区分 (1:資産, 2:負債, 3:純資産, 4:費用, 5:収益)';
COMMENT ON COLUMN accounts.expense_category IS '費用区分';
COMMENT ON COLUMN accounts.display_order IS '表示順序';
COMMENT ON COLUMN accounts.aggregation_target IS '集計対象';
COMMENT ON COLUMN accounts.balance IS '残高';

COMMENT ON COLUMN journal_headers.voucher_number IS '伝票番号';
COMMENT ON COLUMN journal_headers.journal_date IS '仕訳日';
COMMENT ON COLUMN journal_headers.total_amount IS '合計金額';
COMMENT ON COLUMN journal_headers.reference_number IS '参照番号';

COMMENT ON COLUMN journal_details.line_number IS '行番号';
COMMENT ON COLUMN journal_details.debit_amount IS '借方金額';
COMMENT ON COLUMN journal_details.credit_amount IS '貸方金額';

COMMENT ON COLUMN daily_account_balances.balance_date IS '起票日';
COMMENT ON COLUMN daily_account_balances.settlement_flag IS '決算仕訳フラグ';
