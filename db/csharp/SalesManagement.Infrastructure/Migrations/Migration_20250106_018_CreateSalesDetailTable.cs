using FluentMigrator;

namespace SalesManagement.Infrastructure.Migrations
{
    [Migration(20250106018)]
    public class Migration20250106018CreateSalesDetailTable : Migration
    {
        public override void Up()
        {
            Create.Table("売上データ明細")
                .WithColumn("売上番号").AsString(10).NotNullable()
                .WithColumn("売上行番号").AsInt32().NotNullable()
                .WithColumn("商品コード").AsString(20).NotNullable()
                .WithColumn("商品名").AsString(200).NotNullable()
                .WithColumn("販売単価").AsInt32().NotNullable().WithDefaultValue(0)
                .WithColumn("売上数量").AsInt32().NotNullable().WithDefaultValue(1)
                .WithColumn("消費税率").AsInt32().NotNullable().WithDefaultValue(0)
                .WithColumn("値引金額").AsInt32().NotNullable().WithDefaultValue(0)
                .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
                .WithColumn("作成者名").AsString(100).Nullable()
                .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
                .WithColumn("更新者名").AsString(100).Nullable();

            Create.PrimaryKey("pk_売上データ明細")
                .OnTable("売上データ明細")
                .Columns("売上番号", "売上行番号");

            Create.ForeignKey("fk_sales_detail_sales")
                .FromTable("売上データ明細").ForeignColumn("売上番号")
                .ToTable("売上データ").PrimaryColumn("売上番号")
                .OnDelete(System.Data.Rule.Cascade); // CASCADE DELETE

            Create.ForeignKey("fk_sales_detail_product")
                .FromTable("売上データ明細").ForeignColumn("商品コード")
                .ToTable("商品マスタ").PrimaryColumn("商品コード");

            Create.Index("idx_売上データ明細_商品")
                .OnTable("売上データ明細")
                .OnColumn("商品コード");

            IfDatabase("Postgres").Execute.Sql("COMMENT ON TABLE 売上データ明細 IS '売上の商品別明細情報'");
            IfDatabase("Postgres").Execute.Sql("COMMENT ON COLUMN 売上データ明細.商品名 IS '売上時の商品名（履歴保持）'");
            IfDatabase("Postgres").Execute.Sql("COMMENT ON COLUMN 売上データ明細.販売単価 IS '売上時の販売単価（履歴保持）'");
        }

        public override void Down()
        {
            Delete.Table("売上データ明細");
        }
    }
}
