using FluentMigrator;

namespace SalesManagement.Infrastructure.Migrations
{
    /// <summary>
    /// 顧客マスタの作成
    /// </summary>
    [Migration(20250106010)]
    public class Migration20250106010CreateCustomerTable : Migration
    {
        public override void Up()
        {
            Create.Table("顧客マスタ")
                .WithColumn("顧客コード").AsString(8).NotNullable()
                .WithColumn("顧客枝番").AsInt32().NotNullable()
                .WithColumn("顧客区分").AsInt32().NotNullable().WithDefaultValue(0)
                .WithColumn("請求先コード").AsString(8).NotNullable()
                .WithColumn("請求先枝番").AsInt32().Nullable()
                .WithColumn("回収先コード").AsString(8).NotNullable()
                .WithColumn("回収先枝番").AsInt32().Nullable()
                .WithColumn("顧客名").AsString(40).NotNullable()
                .WithColumn("顧客名カナ").AsString(40).Nullable()
                .WithColumn("自社担当者コード").AsString(10).NotNullable()
                .WithColumn("顧客担当者名").AsString(20).Nullable()
                .WithColumn("顧客部門名").AsString(40).Nullable()
                .WithColumn("顧客郵便番号").AsFixedLengthString(8).Nullable()
                .WithColumn("顧客都道府県").AsString(4).Nullable()
                .WithColumn("顧客住所１").AsString(40).Nullable()
                .WithColumn("顧客住所２").AsString(40).Nullable()
                .WithColumn("顧客電話番号").AsString(13).Nullable()
                .WithColumn("顧客ＦＡＸ番号").AsString(13).Nullable()
                .WithColumn("顧客メールアドレス").AsString(100).Nullable()
                .WithColumn("顧客請求区分").AsInt32().NotNullable().WithDefaultValue(0)
                .WithColumn("顧客締日１").AsInt32().NotNullable()
                .WithColumn("顧客支払月１").AsInt32().NotNullable().WithDefaultValue(1)
                .WithColumn("顧客支払日１").AsInt32().Nullable()
                .WithColumn("顧客支払方法１").AsInt32().NotNullable().WithDefaultValue(1)
                .WithColumn("顧客締日２").AsInt32().NotNullable()
                .WithColumn("顧客支払月２").AsInt32().NotNullable().WithDefaultValue(1)
                .WithColumn("顧客支払日２").AsInt32().Nullable()
                .WithColumn("顧客支払方法２").AsInt32().NotNullable().WithDefaultValue(1)
                .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
                .WithColumn("作成者名").AsString(100).Nullable()
                .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
                .WithColumn("更新者名").AsString(100).Nullable();

            Create.PrimaryKey("pk_顧客マスタ")
                .OnTable("顧客マスタ")
                .Columns("顧客コード", "顧客枝番");

            Create.ForeignKey("fk_customer_company")
                .FromTable("顧客マスタ").ForeignColumn("顧客コード")
                .ToTable("取引先マスタ").PrimaryColumn("取引先コード")
                .OnDelete(System.Data.Rule.Cascade);

            Create.ForeignKey("fk_customer_employee")
                .FromTable("顧客マスタ").ForeignColumn("自社担当者コード")
                .ToTable("社員マスタ").PrimaryColumn("社員コード")
                .OnDelete(System.Data.Rule.None);

            Create.Index("idx_顧客マスタ_取引先コード")
                .OnTable("顧客マスタ")
                .OnColumn("顧客コード");

            Execute.Sql("COMMENT ON TABLE 顧客マスタ IS '取引先の顧客としての詳細情報'");
            Execute.Sql("COMMENT ON COLUMN 顧客マスタ.顧客枝番 IS '同一取引先の複数顧客を区別する枝番'");
        }

        public override void Down()
        {
            Delete.Table("顧客マスタ");
        }
    }
}
