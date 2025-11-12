namespace SalesManagement.Infrastructure.Migrations

open FluentMigrator

[<Migration(20250106029L)>]
type Migration_20250106_029_CreateAutoNumberTable() =
    inherit Migration()

    override this.Up() =
        this.Create.Table("自動採番マスタ")
            .WithColumn("伝票種別コード").AsString(2).NotNullable()
            .WithColumn("年月").AsString(6).NotNullable()
            .WithColumn("最終伝票番号").AsInt32().NotNullable().WithDefaultValue(0)
            .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("作成者名").AsString(100).Nullable()
            .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("更新者名").AsString(100).Nullable()
        |> ignore

        this.Create.PrimaryKey("pk_自動採番マスタ")
            .OnTable("自動採番マスタ")
            .Columns("伝票種別コード", "年月")
        |> ignore

        this.Create.Index("idx_自動採番マスタ_年月")
            .OnTable("自動採番マスタ")
            .OnColumn("年月").Ascending()
        |> ignore

        this.Execute.Sql("COMMENT ON TABLE 自動採番マスタ IS '自動採番マスタ'") |> ignore
        this.Execute.Sql("COMMENT ON COLUMN 自動採番マスタ.伝票種別コード IS '伝票種別コード（OR:受注 SA:売上 PO:発注 PU:仕入 IN:請求 CR:入金 PA:支払）'") |> ignore
        this.Execute.Sql("COMMENT ON COLUMN 自動採番マスタ.年月 IS '年月（YYYYMM形式）'") |> ignore
        this.Execute.Sql("COMMENT ON COLUMN 自動採番マスタ.最終伝票番号 IS '最終伝票番号'") |> ignore

    override this.Down() =
        this.Delete.Table("自動採番マスタ") |> ignore
