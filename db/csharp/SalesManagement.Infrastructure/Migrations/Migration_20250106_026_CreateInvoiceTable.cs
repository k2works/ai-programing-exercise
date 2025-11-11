using FluentMigrator;

namespace SalesManagement.Infrastructure.Migrations;

/// <summary>
/// 請求データのマイグレーション
/// </summary>
[Migration(20250106026)]
public class Migration20250106026CreateInvoiceTable : Migration
{
    public override void Up()
    {
        Create.Table("請求データ")
            .WithColumn("請求番号").AsString(20).NotNullable().PrimaryKey()
            .WithColumn("請求日").AsDate().NotNullable()
            .WithColumn("得意先コード").AsString(10).NotNullable()
            .WithColumn("売上番号").AsString(10).Nullable()
            .WithColumn("請求額").AsDecimal(18, 2).NotNullable()
            .WithColumn("請求消込金額").AsDecimal(18, 2).NotNullable().WithDefaultValue(0)
            .WithColumn("備考").AsString(200).Nullable()
            .WithColumn("部門コード").AsString(10).NotNullable()
            .WithColumn("作成日時").AsDateTime().NotNullable()
            .WithColumn("作成者名").AsString(50).NotNullable()
            .WithColumn("更新日時").AsDateTime().NotNullable()
            .WithColumn("更新者名").AsString(50).NotNullable();

        // 外部キー制約
        Create.ForeignKey("fk_invoice_company")
            .FromTable("請求データ").ForeignColumn("得意先コード")
            .ToTable("取引先マスタ").PrimaryColumn("取引先コード");

        Create.ForeignKey("fk_invoice_sales")
            .FromTable("請求データ").ForeignColumn("売上番号")
            .ToTable("売上データ").PrimaryColumn("売上番号");

        // 部門マスタは複合主キー（部門コード、開始日）のため外部キー制約なし

        // インデックス
        Create.Index("idx_invoice_customer")
            .OnTable("請求データ")
            .OnColumn("得意先コード");

        Create.Index("idx_invoice_date")
            .OnTable("請求データ")
            .OnColumn("請求日");
    }

    public override void Down()
    {
        Delete.Table("請求データ");
    }
}
