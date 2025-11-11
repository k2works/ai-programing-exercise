namespace SalesManagement.Infrastructure.Migrations

open FluentMigrator

[<Migration(20250106017L)>]
type Migration_20250106_017_CreateSalesTable() =
    inherit Migration()

    override this.Up() =
        this.Create.Table("売上データ")
            .WithColumn("売上伝票番号").AsString(10).NotNullable().PrimaryKey()
            .WithColumn("売上日").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("受注番号").AsString(10).Nullable()
            .WithColumn("得意先コード").AsString(8).NotNullable()
            .WithColumn("得意先枝番").AsInt32().NotNullable().WithDefaultValue(0)
            .WithColumn("社員コード").AsString(10).NotNullable()
            .WithColumn("売上金額合計").AsInt32().NotNullable().WithDefaultValue(0)
            .WithColumn("消費税合計").AsInt32().NotNullable().WithDefaultValue(0)
            .WithColumn("伝票備考").AsString(1000).Nullable()
            .WithColumn("部門コード").AsString(10).NotNullable()
            .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("作成者名").AsString(100).NotNullable()
            .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("更新者名").AsString(100).NotNullable()
        |> ignore

        this.Create.ForeignKey("fk_sales_customer")
            .FromTable("売上データ").ForeignColumns("得意先コード", "得意先枝番")
            .ToTable("顧客マスタ").PrimaryColumns("顧客コード", "顧客枝番")
        |> ignore

        this.Create.ForeignKey("fk_sales_employee")
            .FromTable("売上データ").ForeignColumn("社員コード")
            .ToTable("社員マスタ").PrimaryColumn("社員コード")
        |> ignore

        // 部門コードへの外部キーは設定しない（部門マスタは複合主キー）

        this.Create.ForeignKey("fk_sales_order")
            .FromTable("売上データ").ForeignColumn("受注番号")
            .ToTable("受注データ").PrimaryColumn("受注番号")
        |> ignore

        this.Create.Index("idx_売上データ_受注番号")
            .OnTable("売上データ")
            .OnColumn("受注番号")
        |> ignore

        this.Create.Index("idx_売上データ_売上日")
            .OnTable("売上データ")
            .OnColumn("売上日").Descending()
        |> ignore

        this.Execute.Sql("COMMENT ON TABLE 売上データ IS '売上情報を管理するヘッダ'")
        this.Execute.Sql("COMMENT ON COLUMN 売上データ.受注番号 IS '元となる受注データへの参照（追跡用）'")

    override this.Down() =
        this.Delete.Table("売上データ") |> ignore
