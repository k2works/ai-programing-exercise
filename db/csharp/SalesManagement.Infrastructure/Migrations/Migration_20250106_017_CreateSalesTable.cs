using FluentMigrator;

namespace SalesManagement.Infrastructure.Migrations
{
    [Migration(20250106017)]
    public class Migration20250106017CreateSalesTable : Migration
    {
        public override void Up()
        {
            Create.Table("売上データ")
                .WithColumn("売上番号").AsString(10).NotNullable().PrimaryKey()
                .WithColumn("売上日").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
                .WithColumn("受注番号").AsString(10).NotNullable()
                .WithColumn("部門コード").AsString(20).NotNullable()
                .WithColumn("開始日").AsDate().NotNullable()
                .WithColumn("取引先コード").AsString(8).NotNullable()
                .WithColumn("社員コード").AsString(20).NotNullable()
                .WithColumn("倉庫コード").AsString(3).NotNullable()
                .WithColumn("売上金額合計").AsInt32().NotNullable().WithDefaultValue(0)
                .WithColumn("消費税合計").AsInt32().NotNullable().WithDefaultValue(0)
                .WithColumn("備考").AsString(1000).Nullable()
                .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
                .WithColumn("作成者名").AsString(100).Nullable()
                .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
                .WithColumn("更新者名").AsString(100).Nullable();

            Create.ForeignKey("fk_sales_order")
                .FromTable("売上データ").ForeignColumn("受注番号")
                .ToTable("受注データ").PrimaryColumn("受注番号");

            Create.ForeignKey("fk_sales_company")
                .FromTable("売上データ").ForeignColumn("取引先コード")
                .ToTable("取引先マスタ").PrimaryColumn("取引先コード");

            Create.ForeignKey("fk_sales_employee")
                .FromTable("売上データ").ForeignColumn("社員コード")
                .ToTable("社員マスタ").PrimaryColumn("社員コード");

            Execute.Sql(@"
                ALTER TABLE 売上データ
                ADD CONSTRAINT fk_sales_department
                FOREIGN KEY (部門コード, 開始日)
                REFERENCES 部門マスタ (部門コード, 開始日)
            ");

            Create.Index("idx_売上データ_受注")
                .OnTable("売上データ")
                .OnColumn("受注番号");

            Create.Index("idx_売上データ_売上日")
                .OnTable("売上データ")
                .OnColumn("売上日");

            Create.Index("idx_売上データ_取引先")
                .OnTable("売上データ")
                .OnColumn("取引先コード");

            Execute.Sql("COMMENT ON TABLE 売上データ IS '売上情報を管理するヘッダ'");
            Execute.Sql("COMMENT ON COLUMN 売上データ.売上番号 IS '売上を一意に識別する番号'");
            Execute.Sql("COMMENT ON COLUMN 売上データ.受注番号 IS '元となった受注番号（追跡可能性）'");
        }

        public override void Down()
        {
            Delete.Table("売上データ");
        }
    }
}
