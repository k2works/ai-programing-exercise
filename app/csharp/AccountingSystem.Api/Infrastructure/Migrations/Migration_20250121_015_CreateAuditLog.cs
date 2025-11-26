using FluentMigrator;

namespace AccountingSystem.Infrastructure.Migrations;

/// <summary>
/// 監査ログテーブルの作成
/// 財務会計システムの監査証跡（Audit Trail）を記録
/// </summary>
[Migration(20250121015)]
public class Migration_20250121_015_CreateAuditLog : Migration
{
    public override void Up()
    {
        // 監査ログテーブル作成
        Execute.Sql(@"
            CREATE TABLE IF NOT EXISTS ""監査ログ"" (
                ""監査ログID"" BIGSERIAL PRIMARY KEY,
                ""エンティティ種別"" VARCHAR(50) NOT NULL,
                ""エンティティID"" VARCHAR(100) NOT NULL,
                ""アクション"" VARCHAR(20) NOT NULL,
                ""ユーザーID"" VARCHAR(100) NOT NULL,
                ""ユーザー名"" VARCHAR(200) NOT NULL,
                ""タイムスタンプ"" TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
                ""変更前値"" JSONB,
                ""変更後値"" JSONB,
                ""変更内容"" JSONB,
                ""理由"" TEXT,
                ""IPアドレス"" VARCHAR(45),
                ""ユーザーエージェント"" TEXT,
                ""作成日時"" TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
            );

            -- インデックス作成
            CREATE INDEX idx_audit_log_entity ON ""監査ログ""(""エンティティ種別"", ""エンティティID"");
            CREATE INDEX idx_audit_log_user ON ""監査ログ""(""ユーザーID"");
            CREATE INDEX idx_audit_log_timestamp ON ""監査ログ""(""タイムスタンプ"");
            CREATE INDEX idx_audit_log_action ON ""監査ログ""(""アクション"");

            -- JSONB 列にもインデックス（特定のキーを検索する場合）
            CREATE INDEX idx_audit_log_changes ON ""監査ログ"" USING GIN (""変更内容"");

            -- コメント
            COMMENT ON TABLE ""監査ログ"" IS '監査ログテーブル（Append-Onlyで不変）';
            COMMENT ON COLUMN ""監査ログ"".""監査ログID"" IS '監査ログID（自動採番）';
            COMMENT ON COLUMN ""監査ログ"".""エンティティ種別"" IS 'エンティティ種別（Journal, Account等）';
            COMMENT ON COLUMN ""監査ログ"".""エンティティID"" IS 'エンティティID';
            COMMENT ON COLUMN ""監査ログ"".""アクション"" IS '操作種別（CREATE, UPDATE, DELETE）';
            COMMENT ON COLUMN ""監査ログ"".""変更前値"" IS '変更前の値（UPDATE, DELETE時）';
            COMMENT ON COLUMN ""監査ログ"".""変更後値"" IS '変更後の値（UPDATE時）';
            COMMENT ON COLUMN ""監査ログ"".""変更内容"" IS '変更内容（CREATE時）';
            COMMENT ON COLUMN ""監査ログ"".""理由"" IS '操作理由（任意）';
        ");
    }

    public override void Down()
    {
        Execute.Sql(@"
            DROP INDEX IF EXISTS idx_audit_log_changes;
            DROP INDEX IF EXISTS idx_audit_log_action;
            DROP INDEX IF EXISTS idx_audit_log_timestamp;
            DROP INDEX IF EXISTS idx_audit_log_user;
            DROP INDEX IF EXISTS idx_audit_log_entity;
            DROP TABLE IF EXISTS ""監査ログ"";
        ");
    }
}
