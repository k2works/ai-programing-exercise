using FluentMigrator;

namespace SalesManagement.Infrastructure.Migrations
{
    /// <summary>
    /// 分類種別マスタの作成
    /// </summary>
    [Migration(20250106012)]
    public class Migration20250106012CreateCategoryTypeTable : Migration
    {
        public override void Up()
        {
            Create.Table("分類種別マスタ")
                .WithColumn("分類種別").AsString(2).NotNullable().PrimaryKey("pk_分類種別マスタ")
                .WithColumn("分類種別名").AsString(20).NotNullable()
                .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
                .WithColumn("作成者名").AsString(100).Nullable()
                .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
                .WithColumn("更新者名").AsString(100).Nullable();

            Execute.Sql("COMMENT ON TABLE 分類種別マスタ IS '取引先を分類する軸を定義するマスタ'");
            Execute.Sql("COMMENT ON COLUMN 分類種別マスタ.分類種別 IS '業種、規模、地域など分類の種類'");
        }

        public override void Down()
        {
            Delete.Table("分類種別マスタ");
        }
    }
}
