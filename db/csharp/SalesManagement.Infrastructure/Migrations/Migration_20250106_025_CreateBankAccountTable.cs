using FluentMigrator;

namespace SalesManagement.Infrastructure.Migrations;

/// <summary>
/// 入金口座マスタのマイグレーション
/// </summary>
[Migration(20250106025)]
public class Migration20250106025CreateBankAccountTable : Migration
{
    public override void Up()
    {
        Create.Table("入金口座マスタ")
            .WithColumn("口座コード").AsString(10).NotNullable().PrimaryKey()
            .WithColumn("口座名").AsString(100).NotNullable()
            .WithColumn("銀行名").AsString(100).NotNullable()
            .WithColumn("支店名").AsString(100).NotNullable()
            .WithColumn("口座番号").AsString(20).NotNullable()
            .WithColumn("口座種別").AsInt32().NotNullable()
            .WithColumn("作成日時").AsDateTime().NotNullable()
            .WithColumn("作成者名").AsString(50).NotNullable()
            .WithColumn("更新日時").AsDateTime().NotNullable()
            .WithColumn("更新者名").AsString(50).NotNullable();
    }

    public override void Down()
    {
        Delete.Table("入金口座マスタ");
    }
}
