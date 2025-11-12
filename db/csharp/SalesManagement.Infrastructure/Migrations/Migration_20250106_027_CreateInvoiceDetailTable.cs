using FluentMigrator;

namespace SalesManagement.Infrastructure.Migrations;

/// <summary>
/// 請求データ明細のマイグレーション
/// </summary>
[Migration(20250106027)]
public class Migration20250106027CreateInvoiceDetailTable : Migration
{
    public override void Up()
    {
        Create.Table("請求データ明細")
            .WithColumn("請求番号").AsString(20).NotNullable()
            .WithColumn("請求明細番号").AsInt32().NotNullable()
            .WithColumn("売上番号").AsString(10).NotNullable()
            .WithColumn("売上行番号").AsInt32().NotNullable()
            .WithColumn("請求額").AsDecimal(18, 2).NotNullable()
            .WithColumn("作成日時").AsDateTime().NotNullable()
            .WithColumn("作成者名").AsString(50).NotNullable()
            .WithColumn("更新日時").AsDateTime().NotNullable()
            .WithColumn("更新者名").AsString(50).NotNullable();

        // 複合主キー
        Create.PrimaryKey("pk_請求データ明細")
            .OnTable("請求データ明細")
            .Columns("請求番号", "請求明細番号");

        // 外部キー制約
        Create.ForeignKey("fk_invoice_detail_invoice")
            .FromTable("請求データ明細").ForeignColumn("請求番号")
            .ToTable("請求データ").PrimaryColumn("請求番号")
            .OnDelete(System.Data.Rule.Cascade);

        // 複合外部キー（売上明細への参照）
        IfDatabase("Postgres").Execute.Sql(@"
            ALTER TABLE 請求データ明細
            ADD CONSTRAINT fk_invoice_detail_sales_detail
            FOREIGN KEY (売上番号, 売上行番号)
            REFERENCES 売上データ明細 (売上番号, 売上行番号)
        ");
    }

    public override void Down()
    {
        Delete.Table("請求データ明細");
    }
}
