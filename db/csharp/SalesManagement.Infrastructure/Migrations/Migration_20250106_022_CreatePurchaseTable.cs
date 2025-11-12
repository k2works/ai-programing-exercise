using FluentMigrator;

namespace SalesManagement.Infrastructure.Migrations
{
    [Migration(20250106022)]
    public class Migration20250106022CreatePurchaseTable : Migration
    {
        public override void Up()
        {
            Create.Table("仕入データ")
                .WithColumn("仕入番号").AsString(10).NotNullable().PrimaryKey()
                .WithColumn("仕入日").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
                .WithColumn("発注番号").AsString(10).NotNullable()
                .WithColumn("仕入先コード").AsString(8).NotNullable()
                .WithColumn("仕入先枝番").AsInt32().NotNullable().WithDefaultValue(0)
                .WithColumn("社員コード").AsString(10).NotNullable()
                .WithColumn("倉庫コード").AsString(3).NotNullable()
                .WithColumn("仕入金額合計").AsInt32().NotNullable().WithDefaultValue(0)
                .WithColumn("消費税合計").AsInt32().NotNullable().WithDefaultValue(0)
                .WithColumn("備考").AsString(1000).Nullable()
                .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
                .WithColumn("作成者名").AsString(100).Nullable()
                .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
                .WithColumn("更新者名").AsString(100).Nullable();

            Create.ForeignKey("fk_purchase_po")
                .FromTable("仕入データ").ForeignColumn("発注番号")
                .ToTable("発注データ").PrimaryColumn("発注番号");

            // 複合外部キー - 仕入先
            Execute.Sql(@"
                ALTER TABLE 仕入データ
                ADD CONSTRAINT fk_purchase_supplier
                FOREIGN KEY (仕入先コード, 仕入先枝番)
                REFERENCES 仕入先マスタ (仕入先コード, 仕入先枝番)
            ");

            Create.ForeignKey("fk_purchase_employee")
                .FromTable("仕入データ").ForeignColumn("社員コード")
                .ToTable("社員マスタ").PrimaryColumn("社員コード");

            Create.ForeignKey("fk_purchase_warehouse")
                .FromTable("仕入データ").ForeignColumn("倉庫コード")
                .ToTable("倉庫マスタ").PrimaryColumn("倉庫コード");

            Create.Index("idx_仕入データ_発注番号")
                .OnTable("仕入データ")
                .OnColumn("発注番号");

            Create.Index("idx_仕入データ_仕入先")
                .OnTable("仕入データ")
                .OnColumn("仕入先コード").Ascending()
                .OnColumn("仕入先枝番").Ascending();

            IfDatabase("Postgres").Execute.Sql("COMMENT ON TABLE 仕入データ IS '仕入情報を管理するヘッダ'");
            IfDatabase("Postgres").Execute.Sql("COMMENT ON COLUMN 仕入データ.発注番号 IS '元となる発注データへの参照'");
        }

        public override void Down()
        {
            Delete.Table("仕入データ");
        }
    }
}
