using FluentMigrator;

namespace SalesManagement.Infrastructure.Migrations
{
    [Migration(20250106016)]
    public class Migration20250106016CreateOrderDetailTable : Migration
    {
        public override void Up()
        {
            Create.Table("受注データ明細")
                .WithColumn("受注番号").AsString(10).NotNullable()
                .WithColumn("受注行番号").AsInt32().NotNullable()
                .WithColumn("商品コード").AsString(20).NotNullable()
                .WithColumn("商品名").AsString(200).NotNullable()
                .WithColumn("販売単価").AsInt32().NotNullable().WithDefaultValue(0)
                .WithColumn("受注数量").AsInt32().NotNullable().WithDefaultValue(1)
                .WithColumn("消費税率").AsInt32().NotNullable().WithDefaultValue(0)
                .WithColumn("引当数量").AsInt32().NotNullable().WithDefaultValue(0)
                .WithColumn("出荷指示数量").AsInt32().NotNullable().WithDefaultValue(0)
                .WithColumn("出荷済数量").AsInt32().NotNullable().WithDefaultValue(0)
                .WithColumn("完了フラグ").AsInt32().NotNullable().WithDefaultValue(0)
                .WithColumn("値引金額").AsInt32().NotNullable().WithDefaultValue(0)
                .WithColumn("納期").AsDateTime().Nullable()
                .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
                .WithColumn("作成者名").AsString(100).Nullable()
                .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
                .WithColumn("更新者名").AsString(100).Nullable();

            Create.PrimaryKey("pk_受注データ明細")
                .OnTable("受注データ明細")
                .Columns("受注番号", "受注行番号");

            Create.ForeignKey("fk_order_detail_order")
                .FromTable("受注データ明細").ForeignColumn("受注番号")
                .ToTable("受注データ").PrimaryColumn("受注番号")
                .OnDelete(System.Data.Rule.Cascade); // CASCADE DELETE

            Create.ForeignKey("fk_order_detail_product")
                .FromTable("受注データ明細").ForeignColumn("商品コード")
                .ToTable("商品マスタ").PrimaryColumn("商品コード");

            Create.Index("idx_受注データ明細_商品")
                .OnTable("受注データ明細")
                .OnColumn("商品コード");

            Execute.Sql("COMMENT ON TABLE 受注データ明細 IS '受注の商品別明細情報'");
            Execute.Sql("COMMENT ON COLUMN 受注データ明細.商品名 IS '受注時の商品名（履歴保持）'");
            Execute.Sql("COMMENT ON COLUMN 受注データ明細.販売単価 IS '受注時の販売単価（履歴保持）'");
            Execute.Sql("COMMENT ON COLUMN 受注データ明細.引当数量 IS '在庫から引き当てた数量'");
        }

        public override void Down()
        {
            Delete.Table("受注データ明細");
        }
    }
}
