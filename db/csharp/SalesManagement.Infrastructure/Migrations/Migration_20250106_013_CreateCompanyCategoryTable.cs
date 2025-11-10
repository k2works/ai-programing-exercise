using FluentMigrator;

namespace SalesManagement.Infrastructure.Migrations
{
    /// <summary>
    /// 取引先分類マスタの作成
    /// </summary>
    [Migration(20250106013)]
    public class Migration20250106013CreateCompanyCategoryTable : Migration
    {
        public override void Up()
        {
            Create.Table("取引先分類マスタ")
                .WithColumn("分類種別").AsString(2).NotNullable()
                .WithColumn("分類").AsString(4).NotNullable()
                .WithColumn("分類名").AsString(40).NotNullable()
                .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
                .WithColumn("作成者名").AsString(100).Nullable()
                .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
                .WithColumn("更新者名").AsString(100).Nullable();

            Create.PrimaryKey("pk_取引先分類マスタ")
                .OnTable("取引先分類マスタ")
                .Columns("分類種別", "分類");

            Create.ForeignKey("fk_company_category_type")
                .FromTable("取引先分類マスタ").ForeignColumn("分類種別")
                .ToTable("分類種別マスタ").PrimaryColumn("分類種別")
                .OnDelete(System.Data.Rule.Cascade);

            Create.Index("idx_取引先分類マスタ_分類種別")
                .OnTable("取引先分類マスタ")
                .OnColumn("分類種別");

            Execute.Sql("COMMENT ON TABLE 取引先分類マスタ IS '分類種別ごとの具体的な分類値を管理'");
            Execute.Sql("COMMENT ON COLUMN 取引先分類マスタ.分類 IS '分類種別内の具体的な分類コード'");
        }

        public override void Down()
        {
            Delete.Table("取引先分類マスタ");
        }
    }
}
