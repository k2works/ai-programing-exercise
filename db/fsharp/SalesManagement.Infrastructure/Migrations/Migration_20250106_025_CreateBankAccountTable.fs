namespace SalesManagement.Infrastructure.Migrations

open FluentMigrator

[<Migration(20250106025L)>]
type Migration_20250106_025_CreateBankAccountTable() =
    inherit Migration()

    override this.Up() =
        this.Create.Table("入金口座マスタ")
            .WithColumn("口座コード").AsString(8).NotNullable().PrimaryKey()
            .WithColumn("口座名").AsString(100).NotNullable()
            .WithColumn("銀行名").AsString(100).NotNullable()
            .WithColumn("支店名").AsString(100).NotNullable()
            .WithColumn("口座番号").AsString(20).NotNullable()
            .WithColumn("口座種別").AsInt32().NotNullable()
            .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("作成者名").AsString(100).NotNullable()
            .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("更新者名").AsString(100).NotNullable()
        |> ignore

        this.Create.Index("idx_入金口座マスタ_銀行名")
            .OnTable("入金口座マスタ")
            .OnColumn("銀行名")
        |> ignore

        this.Execute.Sql("COMMENT ON TABLE 入金口座マスタ IS '入金口座マスタ'") |> ignore
        this.Execute.Sql("COMMENT ON COLUMN 入金口座マスタ.口座種別 IS '口座種別（1:普通 2:当座）'") |> ignore

    override this.Down() =
        this.Delete.Table("入金口座マスタ") |> ignore
