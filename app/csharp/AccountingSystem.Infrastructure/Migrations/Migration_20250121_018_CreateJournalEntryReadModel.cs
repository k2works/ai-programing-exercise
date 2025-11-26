using FluentMigrator;

namespace AccountingSystem.Infrastructure.Migrations;

/// <summary>
/// 仕訳 Read Model テーブルの作成
/// CQRS パターンの Query Side 用
/// </summary>
[Migration(20250121018)]
public class Migration_20250121_018_CreateJournalEntryReadModel : Migration
{
    public override void Up()
    {
        // 仕訳 Read Model テーブル作成
        Execute.Sql(@"
            CREATE TABLE IF NOT EXISTS ""仕訳ReadModel"" (
                -- 主キー
                ""仕訳ID"" VARCHAR(100) PRIMARY KEY,

                -- 仕訳情報
                ""仕訳日"" DATE NOT NULL,
                ""摘要"" TEXT NOT NULL,
                ""ステータス"" VARCHAR(20) NOT NULL,
                ""削除済み"" BOOLEAN NOT NULL DEFAULT false,

                -- タイムスタンプ
                ""作成日時"" TIMESTAMP NOT NULL,
                ""更新日時"" TIMESTAMP NOT NULL,

                -- 承認情報
                ""承認者"" VARCHAR(100),
                ""承認コメント"" TEXT
            );

            -- インデックス
            CREATE INDEX idx_journal_entry_read_model_entry_date
                ON ""仕訳ReadModel""(""仕訳日"");

            CREATE INDEX idx_journal_entry_read_model_status
                ON ""仕訳ReadModel""(""ステータス"");

            CREATE INDEX idx_journal_entry_read_model_deleted
                ON ""仕訳ReadModel""(""削除済み"") WHERE ""削除済み"" = false;

            -- コメント
            COMMENT ON TABLE ""仕訳ReadModel"" IS '仕訳 Read Model（CQRS の Query Side）';
            COMMENT ON COLUMN ""仕訳ReadModel"".""仕訳ID"" IS '仕訳の一意識別子';
            COMMENT ON COLUMN ""仕訳ReadModel"".""仕訳日"" IS '仕訳日';
            COMMENT ON COLUMN ""仕訳ReadModel"".""摘要"" IS '摘要';
            COMMENT ON COLUMN ""仕訳ReadModel"".""ステータス"" IS 'ステータス（DRAFT, APPROVED）';
            COMMENT ON COLUMN ""仕訳ReadModel"".""削除済み"" IS '論理削除フラグ';
            COMMENT ON COLUMN ""仕訳ReadModel"".""作成日時"" IS '作成日時';
            COMMENT ON COLUMN ""仕訳ReadModel"".""更新日時"" IS '更新日時';
            COMMENT ON COLUMN ""仕訳ReadModel"".""承認者"" IS '承認者ID';
            COMMENT ON COLUMN ""仕訳ReadModel"".""承認コメント"" IS '承認コメント';
        ");

        // 仕訳明細 Read Model テーブル作成
        Execute.Sql(@"
            CREATE TABLE IF NOT EXISTS ""仕訳明細ReadModel"" (
                -- 主キー
                ""明細ID"" BIGSERIAL PRIMARY KEY,

                -- 外部キー
                ""仕訳ID"" VARCHAR(100) NOT NULL REFERENCES ""仕訳ReadModel""(""仕訳ID""),

                -- 明細情報
                ""勘定科目コード"" VARCHAR(20) NOT NULL,
                ""貸借区分"" VARCHAR(10) NOT NULL,
                ""金額"" DECIMAL(18, 2) NOT NULL
            );

            -- インデックス
            CREATE INDEX idx_journal_entry_line_read_model_journal_id
                ON ""仕訳明細ReadModel""(""仕訳ID"");

            CREATE INDEX idx_journal_entry_line_read_model_account_code
                ON ""仕訳明細ReadModel""(""勘定科目コード"");

            -- コメント
            COMMENT ON TABLE ""仕訳明細ReadModel"" IS '仕訳明細 Read Model（CQRS の Query Side）';
            COMMENT ON COLUMN ""仕訳明細ReadModel"".""明細ID"" IS '明細の一意識別子';
            COMMENT ON COLUMN ""仕訳明細ReadModel"".""仕訳ID"" IS '仕訳ID（外部キー）';
            COMMENT ON COLUMN ""仕訳明細ReadModel"".""勘定科目コード"" IS '勘定科目コード';
            COMMENT ON COLUMN ""仕訳明細ReadModel"".""貸借区分"" IS '貸借区分（DEBIT, CREDIT）';
            COMMENT ON COLUMN ""仕訳明細ReadModel"".""金額"" IS '金額';
        ");
    }

    public override void Down()
    {
        Execute.Sql(@"
            DROP INDEX IF EXISTS idx_journal_entry_line_read_model_account_code;
            DROP INDEX IF EXISTS idx_journal_entry_line_read_model_journal_id;
            DROP TABLE IF EXISTS ""仕訳明細ReadModel"";

            DROP INDEX IF EXISTS idx_journal_entry_read_model_deleted;
            DROP INDEX IF EXISTS idx_journal_entry_read_model_status;
            DROP INDEX IF EXISTS idx_journal_entry_read_model_entry_date;
            DROP TABLE IF EXISTS ""仕訳ReadModel"";
        ");
    }
}
