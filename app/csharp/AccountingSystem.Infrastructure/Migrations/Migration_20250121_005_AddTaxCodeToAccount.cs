using FluentMigrator;

namespace AccountingSystem.Infrastructure.Migrations;

/// <summary>
/// 勘定科目マスタに課税取引コードを追加
/// </summary>
[Migration(20250121005)]
public class Migration_20250121_005_AddTaxCodeToAccount : Migration
{
    public override void Up()
    {
        // 課税取引コードを追加
        Alter.Table("勘定科目マスタ")
            .AddColumn("課税取引コード").AsString(2).Nullable();

        // コメント追加
        Execute.Sql(@"
            COMMENT ON COLUMN ""勘定科目マスタ"".""課税取引コード"" IS
                '課税取引コード（課税取引マスタへの外部キー）';
        ");

        // インデックスを追加
        Execute.Sql(@"
            CREATE INDEX IF NOT EXISTS idx_account_tax_code
                ON ""勘定科目マスタ"" (""課税取引コード"")
                WHERE ""課税取引コード"" IS NOT NULL;
        ");
    }

    public override void Down()
    {
        Execute.Sql(@"DROP INDEX IF EXISTS idx_account_tax_code;");
        Delete.Column("課税取引コード").FromTable("勘定科目マスタ");
    }
}
