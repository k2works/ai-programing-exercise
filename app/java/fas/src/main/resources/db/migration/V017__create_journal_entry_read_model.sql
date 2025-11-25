-- 仕訳 Read Model テーブル（CQRS パターン）
CREATE TABLE IF NOT EXISTS journal_entry_read_model (
    id VARCHAR(100) PRIMARY KEY,
    entry_date DATE NOT NULL,
    description TEXT NOT NULL,
    status VARCHAR(20) NOT NULL,
    deleted BOOLEAN NOT NULL DEFAULT false,
    created_at TIMESTAMP NOT NULL,
    updated_at TIMESTAMP NOT NULL,
    approved_by VARCHAR(100),
    approval_comment TEXT
);

-- 仕訳明細 Read Model テーブル
CREATE TABLE IF NOT EXISTS journal_entry_line_read_model (
    id BIGSERIAL PRIMARY KEY,
    journal_entry_id VARCHAR(100) NOT NULL REFERENCES journal_entry_read_model(id) ON DELETE CASCADE,
    account_code VARCHAR(20) NOT NULL,
    debit_credit VARCHAR(10) NOT NULL CHECK (debit_credit IN ('DEBIT', 'CREDIT')),
    amount DECIMAL(18, 2) NOT NULL CHECK (amount >= 0)
);

-- インデックス（パフォーマンス最適化）
CREATE INDEX idx_journal_entry_read_model_entry_date
    ON journal_entry_read_model(entry_date);

CREATE INDEX idx_journal_entry_read_model_status
    ON journal_entry_read_model(status);

CREATE INDEX idx_journal_entry_line_read_model_journal_entry_id
    ON journal_entry_line_read_model(journal_entry_id);

CREATE INDEX idx_journal_entry_line_read_model_account_code
    ON journal_entry_line_read_model(account_code);

COMMENT ON TABLE journal_entry_read_model IS '仕訳 Read Model（CQRS パターン）';
COMMENT ON TABLE journal_entry_line_read_model IS '仕訳明細 Read Model';
COMMENT ON COLUMN journal_entry_read_model.id IS '仕訳ID';
COMMENT ON COLUMN journal_entry_read_model.entry_date IS '起票日';
COMMENT ON COLUMN journal_entry_read_model.description IS '摘要';
COMMENT ON COLUMN journal_entry_read_model.status IS 'ステータス（DRAFT/APPROVED）';
COMMENT ON COLUMN journal_entry_read_model.deleted IS '削除フラグ';
COMMENT ON COLUMN journal_entry_read_model.created_at IS '作成日時';
COMMENT ON COLUMN journal_entry_read_model.updated_at IS '更新日時';
COMMENT ON COLUMN journal_entry_read_model.approved_by IS '承認者ID';
COMMENT ON COLUMN journal_entry_read_model.approval_comment IS '承認コメント';
