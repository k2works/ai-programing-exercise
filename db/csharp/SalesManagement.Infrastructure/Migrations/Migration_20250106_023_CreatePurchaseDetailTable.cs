using FluentMigrator;

namespace SalesManagement.Infrastructure.Migrations
{
    [Migration(20250106023)]
    public class Migration20250106023CreatePurchaseDetailTable : Migration
    {
        public override void Up()
        {
            Create.Table("仕入データ明細")
                .WithColumn("仕入番号").AsString(10).NotNullable()
                .WithColumn("仕入行番号").AsInt32().NotNullable()
                .WithColumn("商品コード").AsString(20).NotNullable()
                .WithColumn("商品名").AsString(200).NotNullable()
                .WithColumn("仕入単価").AsInt32().NotNullable().WithDefaultValue(0)
                .WithColumn("仕入数量").AsInt32().NotNullable().WithDefaultValue(1)
                .WithColumn("消費税率").AsInt32().NotNullable().WithDefaultValue(0)
                .WithColumn("ロット番号").AsString(20).NotNullable()
                .WithColumn("倉庫コード").AsString(3).NotNullable()
                .WithColumn("値引金額").AsInt32().NotNullable().WithDefaultValue(0)
                .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
                .WithColumn("作成者名").AsString(100).Nullable()
                .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
                .WithColumn("更新者名").AsString(100).Nullable();

            Create.PrimaryKey("pk_仕入データ明細")
                .OnTable("仕入データ明細")
                .Columns("仕入番号", "仕入行番号");

            Create.ForeignKey("fk_purchase_detail_purchase")
                .FromTable("仕入データ明細").ForeignColumn("仕入番号")
                .ToTable("仕入データ").PrimaryColumn("仕入番号")
                .OnDelete(System.Data.Rule.Cascade);

            Create.ForeignKey("fk_purchase_detail_product")
                .FromTable("仕入データ明細").ForeignColumn("商品コード")
                .ToTable("商品マスタ").PrimaryColumn("商品コード");

            Create.ForeignKey("fk_purchase_detail_warehouse")
                .FromTable("仕入データ明細").ForeignColumn("倉庫コード")
                .ToTable("倉庫マスタ").PrimaryColumn("倉庫コード");

            Create.Index("idx_仕入データ明細_商品")
                .OnTable("仕入データ明細")
                .OnColumn("商品コード");

            Create.Index("idx_仕入データ明細_倉庫")
                .OnTable("仕入データ明細")
                .OnColumn("倉庫コード");

            Create.Index("idx_仕入データ明細_ロット")
                .OnTable("仕入データ明細")
                .OnColumn("ロット番号");

            IfDatabase("Postgres").Execute.Sql("COMMENT ON TABLE 仕入データ明細 IS '仕入の商品別明細情報'");
            IfDatabase("Postgres").Execute.Sql("COMMENT ON COLUMN 仕入データ明細.商品名 IS '仕入時の商品名（履歴保持）'");
            IfDatabase("Postgres").Execute.Sql("COMMENT ON COLUMN 仕入データ明細.仕入単価 IS '仕入時の単価（履歴保持）'");
            IfDatabase("Postgres").Execute.Sql("COMMENT ON COLUMN 仕入データ明細.ロット番号 IS '在庫データと紐づくロット番号'");
        }

        public override void Down()
        {
            Delete.Table("仕入データ明細");
        }
    }
}
