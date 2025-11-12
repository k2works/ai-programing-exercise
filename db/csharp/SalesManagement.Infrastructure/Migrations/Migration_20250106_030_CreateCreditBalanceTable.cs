using FluentMigrator;

namespace SalesManagement.Infrastructure.Migrations;

/// <summary>
/// 与信残高データのマイグレーション
/// </summary>
[Migration(20250106030)]
public class Migration20250106030CreateCreditBalanceTable : Migration
{
    public override void Up()
    {
        Create.Table("与信残高データ")
            .WithColumn("取引先コード").AsString(8).PrimaryKey().NotNullable()
            .WithColumn("受注残高").AsDecimal(15, 2).NotNullable().WithDefaultValue(0)
            .WithColumn("債権残高").AsDecimal(15, 2).NotNullable().WithDefaultValue(0)
            .WithColumn("債務残高").AsDecimal(15, 2).NotNullable().WithDefaultValue(0)
            .WithColumn("作成日時").AsDateTime().NotNullable()
            .WithColumn("作成者名").AsString(100).NotNullable()
            .WithColumn("更新日時").AsDateTime().NotNullable()
            .WithColumn("更新者名").AsString(100).NotNullable();

        Create.ForeignKey("fk_与信残高データ_取引先コード")
            .FromTable("与信残高データ").ForeignColumn("取引先コード")
            .ToTable("取引先マスタ").PrimaryColumn("取引先コード");

        IfDatabase("Postgres").Execute.Sql(@"
            COMMENT ON TABLE 与信残高データ IS '与信残高データ';
            COMMENT ON COLUMN 与信残高データ.取引先コード IS '取引先コード';
            COMMENT ON COLUMN 与信残高データ.受注残高 IS '受注残高（受注したが未出荷の金額）';
            COMMENT ON COLUMN 与信残高データ.債権残高 IS '債権残高（出荷したが未入金の金額）';
            COMMENT ON COLUMN 与信残高データ.債務残高 IS '債務残高（仕入したが未支払の金額）';
        ");

        Create.Index("idx_与信残高データ_受注残高")
            .OnTable("与信残高データ")
            .OnColumn("受注残高");

        Create.Index("idx_与信残高データ_債権残高")
            .OnTable("与信残高データ")
            .OnColumn("債権残高");
    }

    public override void Down()
    {
        Delete.Table("与信残高データ");
    }
}
