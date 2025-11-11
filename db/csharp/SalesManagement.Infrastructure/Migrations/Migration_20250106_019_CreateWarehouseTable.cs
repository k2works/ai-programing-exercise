using FluentMigrator;

namespace SalesManagement.Infrastructure.Migrations
{
    [Migration(20250106019)]
    public class Migration20250106019CreateWarehouseTable : Migration
    {
        public override void Up()
        {
            Create.Table("倉庫マスタ")
                .WithColumn("倉庫コード").AsString(3).NotNullable().PrimaryKey()
                .WithColumn("倉庫名").AsString(100).NotNullable()
                .WithColumn("倉庫区分").AsInt32().NotNullable().WithDefaultValue(1)
                .WithColumn("住所").AsString(200).Nullable()
                .WithColumn("電話番号").AsString(20).Nullable()
                .WithColumn("責任者コード").AsString(10).Nullable()
                .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
                .WithColumn("作成者名").AsString(100).Nullable()
                .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
                .WithColumn("更新者名").AsString(100).Nullable();

            Create.ForeignKey("fk_warehouse_employee")
                .FromTable("倉庫マスタ").ForeignColumn("責任者コード")
                .ToTable("社員マスタ").PrimaryColumn("社員コード");

            Execute.Sql("COMMENT ON TABLE 倉庫マスタ IS '在庫管理の場所を定義するマスタ'");
            Execute.Sql("COMMENT ON COLUMN 倉庫マスタ.倉庫区分 IS '倉庫の種類（1=自社倉庫、2=委託倉庫など）'");
        }

        public override void Down()
        {
            Delete.Table("倉庫マスタ");
        }
    }
}
