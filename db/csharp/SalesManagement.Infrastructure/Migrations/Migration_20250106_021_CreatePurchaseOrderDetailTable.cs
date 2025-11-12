using FluentMigrator;

namespace SalesManagement.Infrastructure.Migrations
{
    [Migration(20250106021)]
    public class Migration20250106021CreatePurchaseOrderDetailTable : Migration
    {
        public override void Up()
        {
            Create.Table("発注データ明細")
                .WithColumn("発注番号").AsString(10).NotNullable()
                .WithColumn("発注行番号").AsInt32().NotNullable()
                .WithColumn("商品コード").AsString(20).NotNullable()
                .WithColumn("商品名").AsString(200).NotNullable()
                .WithColumn("発注単価").AsInt32().NotNullable().WithDefaultValue(0)
                .WithColumn("発注数量").AsInt32().NotNullable().WithDefaultValue(1)
                .WithColumn("消費税率").AsInt32().NotNullable().WithDefaultValue(0)
                .WithColumn("入荷済数量").AsInt32().NotNullable().WithDefaultValue(0)
                .WithColumn("値引金額").AsInt32().NotNullable().WithDefaultValue(0)
                .WithColumn("指定納期").AsDateTime().Nullable()
                .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
                .WithColumn("作成者名").AsString(100).Nullable()
                .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
                .WithColumn("更新者名").AsString(100).Nullable();

            Create.PrimaryKey("pk_発注データ明細")
                .OnTable("発注データ明細")
                .Columns("発注番号", "発注行番号");

            Create.ForeignKey("fk_po_detail_po")
                .FromTable("発注データ明細").ForeignColumn("発注番号")
                .ToTable("発注データ").PrimaryColumn("発注番号")
                .OnDelete(System.Data.Rule.Cascade);

            Create.ForeignKey("fk_po_detail_product")
                .FromTable("発注データ明細").ForeignColumn("商品コード")
                .ToTable("商品マスタ").PrimaryColumn("商品コード");

            Create.Index("idx_発注データ明細_商品")
                .OnTable("発注データ明細")
                .OnColumn("商品コード");

            IfDatabase("Postgres").Execute.Sql("COMMENT ON TABLE 発注データ明細 IS '発注の商品別明細情報'");
            IfDatabase("Postgres").Execute.Sql("COMMENT ON COLUMN 発注データ明細.商品名 IS '発注時の商品名（履歴保持）'");
            IfDatabase("Postgres").Execute.Sql("COMMENT ON COLUMN 発注データ明細.発注単価 IS '発注時の単価（履歴保持）'");
            IfDatabase("Postgres").Execute.Sql("COMMENT ON COLUMN 発注データ明細.入荷済数量 IS '仕入データから入荷済みの数量'");
        }

        public override void Down()
        {
            Delete.Table("発注データ明細");
        }
    }
}
