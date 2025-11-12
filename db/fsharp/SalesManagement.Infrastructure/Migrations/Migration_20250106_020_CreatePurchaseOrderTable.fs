namespace SalesManagement.Infrastructure.Migrations

open FluentMigrator

[<Migration(20250106020L)>]
type Migration_20250106_020_CreatePurchaseOrderTable() =
    inherit Migration()

    override this.Up() =
        this.Create.Table("発注データ")
            .WithColumn("発注番号").AsString(10).NotNullable().PrimaryKey()
            .WithColumn("発注日").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("受注番号").AsString(10).NotNullable()
            .WithColumn("仕入先コード").AsString(8).NotNullable()
            .WithColumn("仕入先枝番").AsInt32().NotNullable().WithDefaultValue(0)
            .WithColumn("社員コード").AsString(10).NotNullable()
            .WithColumn("指定納期").AsDateTime().Nullable()
            .WithColumn("倉庫コード").AsString(10).NotNullable()
            .WithColumn("発注金額合計").AsInt32().NotNullable().WithDefaultValue(0)
            .WithColumn("消費税合計").AsInt32().NotNullable().WithDefaultValue(0)
            .WithColumn("備考").AsString(1000).Nullable()
            .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("作成者名").AsString(100).NotNullable()
            .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("更新者名").AsString(100).NotNullable()
        |> ignore

        this.Create.ForeignKey("fk_purchase_order_order")
            .FromTable("発注データ").ForeignColumn("受注番号")
            .ToTable("受注データ").PrimaryColumn("受注番号")
        |> ignore

        this.Create.ForeignKey("fk_purchase_order_supplier")
            .FromTable("発注データ").ForeignColumns("仕入先コード", "仕入先枝番")
            .ToTable("仕入先マスタ").PrimaryColumns("仕入先コード", "仕入先枝番")
        |> ignore

        this.Create.ForeignKey("fk_purchase_order_employee")
            .FromTable("発注データ").ForeignColumn("社員コード")
            .ToTable("社員マスタ").PrimaryColumn("社員コード")
        |> ignore

        this.Create.ForeignKey("fk_purchase_order_warehouse")
            .FromTable("発注データ").ForeignColumn("倉庫コード")
            .ToTable("倉庫マスタ").PrimaryColumn("倉庫コード")
        |> ignore

        this.Create.Index("idx_発注データ_受注番号")
            .OnTable("発注データ")
            .OnColumn("受注番号").Ascending()
        |> ignore

        this.Create.Index("idx_発注データ_仕入先")
            .OnTable("発注データ")
            .OnColumn("仕入先コード").Ascending()
            .OnColumn("仕入先枝番").Ascending()
        |> ignore

        this.Create.Index("idx_発注データ_発注日")
            .OnTable("発注データ")
            .OnColumn("発注日").Descending()
        |> ignore

        this.Execute.Sql("COMMENT ON TABLE 発注データ IS '仕入先への発注情報を管理するヘッダテーブル'")
        this.Execute.Sql("COMMENT ON COLUMN 発注データ.受注番号 IS 'トレーサビリティのため元の受注番号を記録'")
        this.Execute.Sql("COMMENT ON COLUMN 発注データ.指定納期 IS '仕入先に指定した納品希望日'")

    override this.Down() =
        this.Delete.Table("発注データ") |> ignore
