using FluentMigrator;

namespace SalesManagement.Infrastructure.Migrations
{
    /// <summary>
    /// 取引先グループマスタの作成
    /// </summary>
    [Migration(20250106008)]
    public class Migration20250106008CreateCompanyGroupTable : Migration
    {
        public override void Up()
        {
            Create.Table("取引先グループマスタ")
                .WithColumn("取引先グループコード").AsString(4).NotNullable().PrimaryKey("pk_取引先グループマスタ")
                .WithColumn("取引先グループ名").AsString(40).Nullable()
                .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
                .WithColumn("作成者名").AsString(100).Nullable()
                .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
                .WithColumn("更新者名").AsString(100).Nullable();

            Execute.Sql("COMMENT ON TABLE 取引先グループマスタ IS '取引先をグループ化するマスタ'");
        }

        public override void Down()
        {
            Delete.Table("取引先グループマスタ");
        }
    }
}
