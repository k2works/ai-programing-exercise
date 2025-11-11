using FluentMigrator;

namespace SalesManagement.Infrastructure.Migrations
{
    [Migration(20250106020)]
    public class Migration20250106020CreatePurchaseOrderTable : Migration
    {
        public override void Up()
        {
            Create.Table("発注データ")
                .WithColumn("発注番号").AsString(10).NotNullable().PrimaryKey()
                .WithColumn("発注日").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
                .WithColumn("受注番号").AsString(10).NotNullable()
                .WithColumn("仕入先コード").AsString(8).NotNullable()
                .WithColumn("仕入先枝番").AsInt32().NotNullable().WithDefaultValue(0)
                .WithColumn("社員コード").AsString(10).NotNullable()
                .WithColumn("指定納期").AsDateTime().Nullable()
                .WithColumn("倉庫コード").AsString(3).NotNullable()
                .WithColumn("発注金額合計").AsInt32().NotNullable().WithDefaultValue(0)
                .WithColumn("消費税合計").AsInt32().NotNullable().WithDefaultValue(0)
                .WithColumn("備考").AsString(1000).Nullable()
                .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
                .WithColumn("作成者名").AsString(100).Nullable()
                .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
                .WithColumn("更新者名").AsString(100).Nullable();

            // 複合外部キー - 仕入先
            Execute.Sql(@"
                ALTER TABLE 発注データ
                ADD CONSTRAINT fk_po_supplier
                FOREIGN KEY (仕入先コード, 仕入先枝番)
                REFERENCES 仕入先マスタ (仕入先コード, 仕入先枝番)
            ");

            Create.ForeignKey("fk_po_employee")
                .FromTable("発注データ").ForeignColumn("社員コード")
                .ToTable("社員マスタ").PrimaryColumn("社員コード");

            Create.ForeignKey("fk_po_order")
                .FromTable("発注データ").ForeignColumn("受注番号")
                .ToTable("受注データ").PrimaryColumn("受注番号");

            Create.ForeignKey("fk_po_warehouse")
                .FromTable("発注データ").ForeignColumn("倉庫コード")
                .ToTable("倉庫マスタ").PrimaryColumn("倉庫コード");

            Create.Index("idx_発注データ_仕入先")
                .OnTable("発注データ")
                .OnColumn("仕入先コード").Ascending()
                .OnColumn("仕入先枝番").Ascending();

            Create.Index("idx_発注データ_受注番号")
                .OnTable("発注データ")
                .OnColumn("受注番号");

            Execute.Sql("COMMENT ON TABLE 発注データ IS '仕入先への発注情報を管理するヘッダ'");
            Execute.Sql("COMMENT ON COLUMN 発注データ.受注番号 IS '元となる受注データへの参照'");
        }

        public override void Down()
        {
            Delete.Table("発注データ");
        }
    }
}
