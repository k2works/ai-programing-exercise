using FluentMigrator;

namespace SalesManagement.Infrastructure.Migrations
{
    /// <summary>
    /// 取引先分類所属マスタの作成
    /// </summary>
    [Migration(20250106014)]
    public class Migration20250106014CreateCompanyCategoryGroupTable : Migration
    {
        public override void Up()
        {
            Create.Table("取引先分類所属マスタ")
                .WithColumn("分類種別").AsString(2).NotNullable()
                .WithColumn("分類").AsString(4).NotNullable()
                .WithColumn("取引先コード").AsString(8).NotNullable()
                .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
                .WithColumn("作成者名").AsString(100).Nullable()
                .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
                .WithColumn("更新者名").AsString(100).Nullable();

            Create.PrimaryKey("pk_取引先分類所属マスタ")
                .OnTable("取引先分類所属マスタ")
                .Columns("分類種別", "分類", "取引先コード");

            Execute.Sql(@"
                ALTER TABLE 取引先分類所属マスタ
                ADD CONSTRAINT fk_company_category_group_category
                FOREIGN KEY (分類種別, 分類)
                REFERENCES 取引先分類マスタ (分類種別, 分類)
                ON DELETE CASCADE
            ");

            Create.ForeignKey("fk_company_category_group_company")
                .FromTable("取引先分類所属マスタ").ForeignColumn("取引先コード")
                .ToTable("取引先マスタ").PrimaryColumn("取引先コード")
                .OnDelete(System.Data.Rule.Cascade);

            Create.Index("idx_取引先分類所属マスタ_取引先")
                .OnTable("取引先分類所属マスタ")
                .OnColumn("取引先コード");

            Execute.Sql("COMMENT ON TABLE 取引先分類所属マスタ IS '取引先と分類の多対多関係を管理'");
        }

        public override void Down()
        {
            Delete.Table("取引先分類所属マスタ");
        }
    }
}
