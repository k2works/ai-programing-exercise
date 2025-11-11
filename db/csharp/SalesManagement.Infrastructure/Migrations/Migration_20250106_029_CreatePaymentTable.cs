using FluentMigrator;

namespace SalesManagement.Infrastructure.Migrations;

/// <summary>
/// 支払データのマイグレーション
/// </summary>
[Migration(20250106029)]
public class Migration20250106029CreatePaymentTable : Migration
{
    public override void Up()
    {
        Create.Table("支払データ")
            .WithColumn("支払伝票番号").AsString(20).NotNullable().PrimaryKey()
            .WithColumn("支払日").AsDate().NotNullable()
            .WithColumn("仕入先コード").AsString(10).NotNullable()
            .WithColumn("支払額").AsDecimal(18, 2).NotNullable()
            .WithColumn("支払方法").AsInt32().NotNullable()
            .WithColumn("支払完了フラグ").AsInt32().NotNullable().WithDefaultValue(0)
            .WithColumn("備考").AsString(200).Nullable()
            .WithColumn("部門コード").AsString(10).NotNullable()
            .WithColumn("作成日時").AsDateTime().NotNullable()
            .WithColumn("作成者名").AsString(50).NotNullable()
            .WithColumn("更新日時").AsDateTime().NotNullable()
            .WithColumn("更新者名").AsString(50).NotNullable();

        // 外部キー制約
        Create.ForeignKey("fk_payment_company")
            .FromTable("支払データ").ForeignColumn("仕入先コード")
            .ToTable("取引先マスタ").PrimaryColumn("取引先コード");

        // 部門マスタは複合主キー（部門コード、開始日）のため外部キー制約なし

        // インデックス
        Create.Index("idx_payment_supplier")
            .OnTable("支払データ")
            .OnColumn("仕入先コード");

        Create.Index("idx_payment_date")
            .OnTable("支払データ")
            .OnColumn("支払日");

        Create.Index("idx_payment_completed")
            .OnTable("支払データ")
            .OnColumn("支払完了フラグ");
    }

    public override void Down()
    {
        Delete.Table("支払データ");
    }
}
