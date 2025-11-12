namespace SalesManagement.Infrastructure.Migrations

open FluentMigrator

[<Migration(20250106027L)>]
type Migration_20250106_027_CreateInvoiceDetailTable() =
    inherit Migration()

    override this.Up() =
        this.Create.Table("請求データ明細")
            .WithColumn("請求番号").AsString(13).NotNullable()
            .WithColumn("請求明細番号").AsInt32().NotNullable()
            .WithColumn("売上伝票番号").AsString(13).NotNullable()
            .WithColumn("売上明細番号").AsInt32().NotNullable()
            .WithColumn("請求額").AsDecimal(15, 2).NotNullable()
            .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("作成者名").AsString(100).NotNullable()
            .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("更新者名").AsString(100).NotNullable()
        |> ignore

        this.Create.PrimaryKey("pk_請求データ明細")
            .OnTable("請求データ明細")
            .Columns("請求番号", "請求明細番号")
        |> ignore

        this.Create.ForeignKey("fk_invoice_detail_invoice")
            .FromTable("請求データ明細").ForeignColumn("請求番号")
            .ToTable("請求データ").PrimaryColumn("請求番号")
        |> ignore

        this.Create.ForeignKey("fk_invoice_detail_sales_detail")
            .FromTable("請求データ明細").ForeignColumns("売上伝票番号", "売上明細番号")
            .ToTable("売上データ明細").PrimaryColumns("売上伝票番号", "売上行番号")
        |> ignore

        this.Create.Index("idx_請求データ明細_売上明細")
            .OnTable("請求データ明細")
            .OnColumn("売上伝票番号").Ascending()
            .OnColumn("売上明細番号").Ascending()
        |> ignore

        this.Execute.Sql("COMMENT ON TABLE 請求データ明細 IS '請求データ明細'") |> ignore

    override this.Down() =
        this.Delete.Table("請求データ明細") |> ignore
