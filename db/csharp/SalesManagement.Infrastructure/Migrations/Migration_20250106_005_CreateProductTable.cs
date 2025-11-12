using FluentMigrator;

namespace SalesManagement.Infrastructure.Migrations
{
    /// <summary>
    /// 商品マスタテーブルの作成
    /// </summary>
    [Migration(20250106005)]
    public class Migration20250106005CreateProductTable : Migration
    {
        public override void Up()
        {
            Create.Table("商品マスタ")
                .WithColumn("商品コード").AsString(20).NotNullable().PrimaryKey("pk_商品マスタ")
                .WithColumn("商品正式名").AsString(100).NotNullable()
                .WithColumn("商品略称").AsString(50).NotNullable()
                .WithColumn("商品名カナ").AsString(100).Nullable()
                .WithColumn("商品区分").AsString(10).NotNullable()
                .WithColumn("製品型番").AsString(50).Nullable()
                .WithColumn("販売単価").AsInt32().NotNullable().WithDefaultValue(0)
                .WithColumn("仕入単価").AsInt32().NotNullable().WithDefaultValue(0)
                .WithColumn("売上原価").AsInt32().NotNullable().WithDefaultValue(0)
                .WithColumn("税区分").AsInt32().NotNullable().WithDefaultValue(1)
                .WithColumn("商品分類コード").AsString(20).NotNullable()
                .WithColumn("雑区分").AsInt32().NotNullable().WithDefaultValue(0)
                .WithColumn("在庫管理対象区分").AsInt32().NotNullable().WithDefaultValue(1)
                .WithColumn("在庫引当区分").AsInt32().NotNullable().WithDefaultValue(0)
                .WithColumn("仕入先コード").AsString(20).Nullable()
                .WithColumn("仕入先枝番").AsInt32().Nullable()
                .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
                .WithColumn("作成者名").AsString(50).NotNullable()
                .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
                .WithColumn("更新者名").AsString(50).NotNullable();

            Create.Index("idx_商品マスタ_商品分類コード")
                .OnTable("商品マスタ")
                .OnColumn("商品分類コード");

            Create.Index("idx_商品マスタ_商品名カナ")
                .OnTable("商品マスタ")
                .OnColumn("商品名カナ");

            Create.ForeignKey("fk_商品マスタ_商品分類コード")
                .FromTable("商品マスタ").ForeignColumn("商品分類コード")
                .ToTable("商品分類マスタ").PrimaryColumn("商品分類コード")
                .OnDelete(System.Data.Rule.None);

            // テーブルコメント（PostgreSQL）
            IfDatabase("Postgres").Execute.Sql("COMMENT ON TABLE 商品マスタ IS '商品の基本情報を管理するマスタ'");
            IfDatabase("Postgres").Execute.Sql("COMMENT ON COLUMN 商品マスタ.商品コード IS '商品の一意識別子'");
            IfDatabase("Postgres").Execute.Sql("COMMENT ON COLUMN 商品マスタ.販売単価 IS '標準販売単価'");
            IfDatabase("Postgres").Execute.Sql("COMMENT ON COLUMN 商品マスタ.在庫管理対象区分 IS '在庫管理が必要か（0:不要, 1:必要）'");
        }

        public override void Down()
        {
            Delete.Table("商品マスタ");
        }
    }
}
