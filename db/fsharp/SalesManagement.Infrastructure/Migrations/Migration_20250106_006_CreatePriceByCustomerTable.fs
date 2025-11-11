namespace SalesManagement.Infrastructure.Migrations

open FluentMigrator

[<Migration(20250106006L)>]
type Migration_20250106_006_CreatePriceByCustomerTable() =
    inherit Migration()

    override this.Up() =
        this.Create.Table("顧客別販売単価")
            .WithColumn("商品コード").AsString(20).NotNullable()
            .WithColumn("取引先コード").AsString(20).NotNullable()
            .WithColumn("販売単価").AsInt32().NotNullable()
            .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("作成者名").AsString(50).NotNullable()
            .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("更新者名").AsString(50).NotNullable()
        |> ignore

        this.Create.PrimaryKey("pk_顧客別販売単価")
            .OnTable("顧客別販売単価")
            .Columns("商品コード", "取引先コード")
        |> ignore

        this.Create.Index("idx_顧客別販売単価_取引先コード")
            .OnTable("顧客別販売単価")
            .OnColumn("取引先コード")
        |> ignore

        this.Create.ForeignKey("fk_顧客別販売単価_商品マスタ")
            .FromTable("顧客別販売単価").ForeignColumn("商品コード")
            .ToTable("商品マスタ").PrimaryColumn("商品コード")
        |> ignore

        this.Execute.Sql("COMMENT ON TABLE 顧客別販売単価 IS '顧客ごとの特別販売単価を管理'")
        this.Execute.Sql("COMMENT ON COLUMN 顧客別販売単価.販売単価 IS 'この顧客への販売単価'")

    override this.Down() =
        this.Delete.Table("顧客別販売単価") |> ignore
