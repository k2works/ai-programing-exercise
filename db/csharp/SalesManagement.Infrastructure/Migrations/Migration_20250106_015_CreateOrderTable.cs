using FluentMigrator;

namespace SalesManagement.Infrastructure.Migrations
{
    [Migration(20250106015)]
    public class Migration20250106015CreateOrderTable : Migration
    {
        public override void Up()
        {
            Create.Table("受注データ")
                .WithColumn("受注番号").AsString(10).NotNullable().PrimaryKey()
                .WithColumn("受注日").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
                .WithColumn("部門コード").AsString(20).NotNullable()
                .WithColumn("開始日").AsDate().NotNullable()
                .WithColumn("顧客コード").AsString(8).NotNullable()
                .WithColumn("顧客枝番").AsInt32().Nullable()
                .WithColumn("社員コード").AsString(20).NotNullable()
                .WithColumn("希望納期").AsDateTime().Nullable()
                .WithColumn("客先注文番号").AsString(20).Nullable()
                .WithColumn("倉庫コード").AsString(3).NotNullable()
                .WithColumn("受注金額合計").AsInt32().NotNullable().WithDefaultValue(0)
                .WithColumn("消費税合計").AsInt32().NotNullable().WithDefaultValue(0)
                .WithColumn("備考").AsString(1000).Nullable()
                .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
                .WithColumn("作成者名").AsString(100).Nullable()
                .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
                .WithColumn("更新者名").AsString(100).Nullable();

            // 複合外部キーは Execute.Sql で実装
            Execute.Sql(@"
                ALTER TABLE 受注データ
                ADD CONSTRAINT fk_order_customer
                FOREIGN KEY (顧客コード, 顧客枝番)
                REFERENCES 顧客マスタ (顧客コード, 顧客枝番)
            ");

            Create.ForeignKey("fk_order_employee")
                .FromTable("受注データ").ForeignColumn("社員コード")
                .ToTable("社員マスタ").PrimaryColumn("社員コード");

            Execute.Sql(@"
                ALTER TABLE 受注データ
                ADD CONSTRAINT fk_order_department
                FOREIGN KEY (部門コード, 開始日)
                REFERENCES 部門マスタ (部門コード, 開始日)
            ");

            Create.Index("idx_受注データ_顧客")
                .OnTable("受注データ")
                .OnColumn("顧客コード").Ascending()
                .OnColumn("顧客枝番").Ascending();

            Create.Index("idx_受注データ_受注日")
                .OnTable("受注データ")
                .OnColumn("受注日");

            IfDatabase("Postgres").Execute.Sql("COMMENT ON TABLE 受注データ IS '顧客からの注文情報を管理するヘッダ'");
            IfDatabase("Postgres").Execute.Sql("COMMENT ON COLUMN 受注データ.受注番号 IS '受注を一意に識別する番号'");
            IfDatabase("Postgres").Execute.Sql("COMMENT ON COLUMN 受注データ.希望納期 IS '顧客が希望する納期'");
        }

        public override void Down()
        {
            Delete.Table("受注データ");
        }
    }
}
