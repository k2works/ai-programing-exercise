-- 仕訳の読み取りモデル（CQRS）
CREATE TABLE IF NOT EXISTS journal_read_model (
    journal_id VARCHAR(255) PRIMARY KEY,
    journal_date DATE NOT NULL,
    description TEXT NOT NULL,
    fiscal_year INT NOT NULL,
    status VARCHAR(20) NOT NULL,
    total_debit BIGINT NOT NULL DEFAULT 0,
    total_credit BIGINT NOT NULL DEFAULT 0,
    entry_count INT NOT NULL DEFAULT 0,
    approved_by VARCHAR(100),
    approved_at TIMESTAMP WITH TIME ZONE,
    created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
);

-- インデックス（クエリパフォーマンス最適化）
CREATE INDEX idx_journal_read_status ON journal_read_model(status);
CREATE INDEX idx_journal_read_date ON journal_read_model(journal_date);
CREATE INDEX idx_journal_read_fiscal_year ON journal_read_model(fiscal_year);
CREATE INDEX idx_journal_read_created_at ON journal_read_model(created_at DESC);

-- 仕訳明細の読み取りモデル
CREATE TABLE IF NOT EXISTS journal_entry_read_model (
    entry_id BIGSERIAL PRIMARY KEY,
    journal_id VARCHAR(255) NOT NULL REFERENCES journal_read_model(journal_id) ON DELETE CASCADE,
    account_code VARCHAR(10) NOT NULL,
    account_name VARCHAR(100),
    debit_amount BIGINT,
    credit_amount BIGINT,
    description TEXT,
    line_number INT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
);

CREATE INDEX idx_entry_read_journal ON journal_entry_read_model(journal_id);
CREATE INDEX idx_entry_read_account ON journal_entry_read_model(account_code);

-- コメント
COMMENT ON TABLE journal_read_model IS 'CQRS読み取りモデル：仕訳一覧・検索用';
COMMENT ON COLUMN journal_read_model.journal_id IS '仕訳ID（集約ID）';
COMMENT ON COLUMN journal_read_model.status IS 'ステータス（draft/approved/canceled）';
COMMENT ON COLUMN journal_read_model.total_debit IS '借方合計';
COMMENT ON COLUMN journal_read_model.total_credit IS '貸方合計';
COMMENT ON COLUMN journal_read_model.entry_count IS '明細数';

COMMENT ON TABLE journal_entry_read_model IS 'CQRS読み取りモデル：仕訳明細表示用';
COMMENT ON COLUMN journal_entry_read_model.debit_amount IS '借方金額（借方の場合のみ）';
COMMENT ON COLUMN journal_entry_read_model.credit_amount IS '貸方金額（貸方の場合のみ）';
