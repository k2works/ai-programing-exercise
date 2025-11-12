using FluentMigrator;

namespace SalesManagement.Infrastructure.Migrations
{
    /// <summary>
    /// 仕入先マスタの作成
    /// </summary>
    [Migration(20250106011)]
    public class Migration20250106011CreateSupplierTable : Migration
    {
        public override void Up()
        {
            Create.Table("仕入先マスタ")
                .WithColumn("仕入先コード").AsString(8).NotNullable()
                .WithColumn("仕入先枝番").AsInt32().NotNullable()
                .WithColumn("仕入先名").AsString(40).NotNullable()
                .WithColumn("仕入先名カナ").AsString(40).Nullable()
                .WithColumn("仕入先担当者名").AsString(20).Nullable()
                .WithColumn("仕入先部門名").AsString(40).Nullable()
                .WithColumn("仕入先郵便番号").AsFixedLengthString(8).Nullable()
                .WithColumn("仕入先都道府県").AsString(4).Nullable()
                .WithColumn("仕入先住所１").AsString(40).Nullable()
                .WithColumn("仕入先住所２").AsString(40).Nullable()
                .WithColumn("仕入先電話番号").AsString(13).Nullable()
                .WithColumn("仕入先ＦＡＸ番号").AsString(13).Nullable()
                .WithColumn("仕入先メールアドレス").AsString(100).Nullable()
                .WithColumn("仕入先締日").AsInt32().NotNullable()
                .WithColumn("仕入先支払月").AsInt32().NotNullable().WithDefaultValue(1)
                .WithColumn("仕入先支払日").AsInt32().Nullable()
                .WithColumn("仕入先支払方法").AsInt32().NotNullable().WithDefaultValue(1)
                .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
                .WithColumn("作成者名").AsString(100).Nullable()
                .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
                .WithColumn("更新者名").AsString(100).Nullable();

            Create.PrimaryKey("pk_仕入先マスタ")
                .OnTable("仕入先マスタ")
                .Columns("仕入先コード", "仕入先枝番");

            Create.ForeignKey("fk_supplier_company")
                .FromTable("仕入先マスタ").ForeignColumn("仕入先コード")
                .ToTable("取引先マスタ").PrimaryColumn("取引先コード")
                .OnDelete(System.Data.Rule.Cascade);

            Create.Index("idx_仕入先マスタ_取引先コード")
                .OnTable("仕入先マスタ")
                .OnColumn("仕入先コード");

            IfDatabase("Postgres").Execute.Sql("COMMENT ON TABLE 仕入先マスタ IS '取引先の仕入先としての詳細情報'");
            IfDatabase("Postgres").Execute.Sql("COMMENT ON COLUMN 仕入先マスタ.仕入先枝番 IS '同一取引先の複数仕入先を区別する枝番'");
        }

        public override void Down()
        {
            Delete.Table("仕入先マスタ");
        }
    }
}
