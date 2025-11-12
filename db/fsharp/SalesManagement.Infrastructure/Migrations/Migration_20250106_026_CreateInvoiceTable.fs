namespace SalesManagement.Infrastructure.Migrations

open FluentMigrator

[<Migration(20250106026L)>]
type Migration_20250106_026_CreateInvoiceTable() =
    inherit Migration()

    override this.Up() =
        this.Create.Table("請求データ")
            .WithColumn("請求番号").AsString(13).NotNullable().PrimaryKey()
            .WithColumn("請求日").AsDate().NotNullable()
            .WithColumn("得意先コード").AsString(8).NotNullable()
            .WithColumn("得意先枝番").AsInt32().NotNullable().WithDefaultValue(0)
            .WithColumn("売上伝票番号").AsString(13).Nullable()
            .WithColumn("請求額").AsDecimal(15, 2).NotNullable()
            .WithColumn("請求消込金額").AsDecimal(15, 2).NotNullable().WithDefaultValue(0)
            .WithColumn("備考").AsString(200).Nullable()
            .WithColumn("部門コード").AsString(10).NotNullable()
            .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("作成者名").AsString(100).NotNullable()
            .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("更新者名").AsString(100).NotNullable()
        |> ignore

        this.Create.ForeignKey("fk_invoice_customer")
            .FromTable("請求データ").ForeignColumns("得意先コード", "得意先枝番")
            .ToTable("顧客マスタ").PrimaryColumns("顧客コード", "顧客枝番")
        |> ignore

        this.Create.ForeignKey("fk_invoice_sales")
            .FromTable("請求データ").ForeignColumn("売上伝票番号")
            .ToTable("売上データ").PrimaryColumn("売上伝票番号")
        |> ignore

        this.Create.Index("idx_請求データ_得意先")
            .OnTable("請求データ")
            .OnColumn("得意先コード").Ascending()
            .OnColumn("得意先枝番").Ascending()
        |> ignore

        this.Create.Index("idx_請求データ_請求日")
            .OnTable("請求データ")
            .OnColumn("請求日")
        |> ignore

        this.Execute.Sql("COMMENT ON TABLE 請求データ IS '請求データ'") |> ignore
        this.Execute.Sql("COMMENT ON COLUMN 請求データ.請求消込金額 IS '入金により消し込まれた金額'") |> ignore

    override this.Down() =
        this.Delete.Table("請求データ") |> ignore
