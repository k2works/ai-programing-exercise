using FluentMigrator;

namespace SalesManagement.Infrastructure.Migrations
{
    [Migration(20250106024)]
    public class Migration20250106024CreateStockTable : Migration
    {
        public override void Up()
        {
            Create.Table("在庫データ")
                .WithColumn("倉庫コード").AsString(3).NotNullable()
                .WithColumn("商品コード").AsString(20).NotNullable()
                .WithColumn("ロット番号").AsString(20).NotNullable()
                .WithColumn("在庫区分").AsString(1).NotNullable().WithDefaultValue("1")
                .WithColumn("良品区分").AsString(1).NotNullable().WithDefaultValue("G")
                .WithColumn("実在庫数").AsInt32().NotNullable().WithDefaultValue(0)
                .WithColumn("有効在庫数").AsInt32().NotNullable().WithDefaultValue(0)
                .WithColumn("最終出荷日").AsDateTime().Nullable()
                .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
                .WithColumn("作成者名").AsString(100).Nullable()
                .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
                .WithColumn("更新者名").AsString(100).Nullable();

            Create.PrimaryKey("pk_在庫データ")
                .OnTable("在庫データ")
                .Columns("倉庫コード", "商品コード", "ロット番号", "在庫区分", "良品区分");

            Create.ForeignKey("fk_stock_warehouse")
                .FromTable("在庫データ").ForeignColumn("倉庫コード")
                .ToTable("倉庫マスタ").PrimaryColumn("倉庫コード");

            Create.ForeignKey("fk_stock_product")
                .FromTable("在庫データ").ForeignColumn("商品コード")
                .ToTable("商品マスタ").PrimaryColumn("商品コード");

            Create.Index("idx_在庫データ_倉庫")
                .OnTable("在庫データ")
                .OnColumn("倉庫コード");

            Create.Index("idx_在庫データ_商品")
                .OnTable("在庫データ")
                .OnColumn("商品コード");

            Create.Index("idx_在庫データ_ロット")
                .OnTable("在庫データ")
                .OnColumn("ロット番号");

            IfDatabase("Postgres").Execute.Sql("COMMENT ON TABLE 在庫データ IS '倉庫別・商品別・ロット別の在庫数量を管理'");
            IfDatabase("Postgres").Execute.Sql("COMMENT ON COLUMN 在庫データ.在庫区分 IS '在庫の種類（1=通常在庫、2=預託在庫など）'");
            IfDatabase("Postgres").Execute.Sql("COMMENT ON COLUMN 在庫データ.良品区分 IS '品質区分（G=良品、B=不良品、H=保留品）'");
            IfDatabase("Postgres").Execute.Sql("COMMENT ON COLUMN 在庫データ.実在庫数 IS '物理的に存在する在庫数'");
            IfDatabase("Postgres").Execute.Sql("COMMENT ON COLUMN 在庫データ.有効在庫数 IS '販売可能な在庫数（実在庫 - 引当数量）'");
        }

        public override void Down()
        {
            Delete.Table("在庫データ");
        }
    }
}
