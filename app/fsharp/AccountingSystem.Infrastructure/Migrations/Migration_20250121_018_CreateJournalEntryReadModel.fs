namespace AccountingSystem.Infrastructure.Migrations

open FluentMigrator

/// <summary>
/// 仕訳 Read Model テーブルの作成
/// CQRS パターンの読み取りモデル
/// </summary>
[<Migration(20250121018L)>]
type Migration_20250121_018_CreateJournalEntryReadModel() =
    inherit Migration()

    override this.Up() =
        // 仕訳 Read Model テーブル
        this.Execute.Sql("""
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

            -- テーブルコメント
            COMMENT ON TABLE journal_entry_read_model IS '仕訳Read Model - CQRS読み取り用テーブル';
            COMMENT ON COLUMN journal_entry_read_model.id IS '仕訳ID';
            COMMENT ON COLUMN journal_entry_read_model.entry_date IS '仕訳日';
            COMMENT ON COLUMN journal_entry_read_model.description IS '摘要';
            COMMENT ON COLUMN journal_entry_read_model.status IS 'ステータス（DRAFT/APPROVED）';
            COMMENT ON COLUMN journal_entry_read_model.deleted IS '削除フラグ';
            COMMENT ON COLUMN journal_entry_read_model.created_at IS '作成日時';
            COMMENT ON COLUMN journal_entry_read_model.updated_at IS '更新日時';
            COMMENT ON COLUMN journal_entry_read_model.approved_by IS '承認者';
            COMMENT ON COLUMN journal_entry_read_model.approval_comment IS '承認コメント';
        """)

        // 仕訳明細 Read Model テーブル
        this.Execute.Sql("""
            CREATE TABLE IF NOT EXISTS journal_entry_line_read_model (
                id BIGSERIAL PRIMARY KEY,
                journal_entry_id VARCHAR(100) NOT NULL REFERENCES journal_entry_read_model(id),
                account_code VARCHAR(20) NOT NULL,
                debit_credit VARCHAR(10) NOT NULL,
                amount DECIMAL(18, 2) NOT NULL
            );

            -- テーブルコメント
            COMMENT ON TABLE journal_entry_line_read_model IS '仕訳明細Read Model - CQRS読み取り用テーブル';
            COMMENT ON COLUMN journal_entry_line_read_model.id IS '明細ID';
            COMMENT ON COLUMN journal_entry_line_read_model.journal_entry_id IS '仕訳ID（外部キー）';
            COMMENT ON COLUMN journal_entry_line_read_model.account_code IS '勘定科目コード';
            COMMENT ON COLUMN journal_entry_line_read_model.debit_credit IS '借方/貸方区分';
            COMMENT ON COLUMN journal_entry_line_read_model.amount IS '金額';
        """)

        // インデックスの作成
        this.Execute.Sql("""
            CREATE INDEX IF NOT EXISTS idx_journal_entry_read_model_entry_date
                ON journal_entry_read_model(entry_date);

            CREATE INDEX IF NOT EXISTS idx_journal_entry_read_model_status
                ON journal_entry_read_model(status);

            CREATE INDEX IF NOT EXISTS idx_journal_entry_read_model_deleted
                ON journal_entry_read_model(deleted);

            CREATE INDEX IF NOT EXISTS idx_journal_entry_line_read_model_journal_entry_id
                ON journal_entry_line_read_model(journal_entry_id);

            CREATE INDEX IF NOT EXISTS idx_journal_entry_line_read_model_account_code
                ON journal_entry_line_read_model(account_code);
        """)

    override this.Down() =
        this.Execute.Sql("""
            DROP INDEX IF EXISTS idx_journal_entry_line_read_model_account_code;
            DROP INDEX IF EXISTS idx_journal_entry_line_read_model_journal_entry_id;
            DROP INDEX IF EXISTS idx_journal_entry_read_model_deleted;
            DROP INDEX IF EXISTS idx_journal_entry_read_model_status;
            DROP INDEX IF EXISTS idx_journal_entry_read_model_entry_date;
            DROP TABLE IF EXISTS journal_entry_line_read_model;
            DROP TABLE IF EXISTS journal_entry_read_model;
        """)
