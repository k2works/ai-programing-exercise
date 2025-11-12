namespace SalesManagement.Infrastructure.Migrations

open FluentMigrator

[<Migration(20250106028L)>]
type Migration_20250106_028_CreateCreditBalanceTable() =
    inherit Migration()

    override this.Up() =
        this.Create.Table("与信残高データ")
            .WithColumn("取引先コード").AsString(8).NotNullable().PrimaryKey()
            .WithColumn("受注残高").AsDecimal(15, 2).NotNullable().WithDefaultValue(0)
            .WithColumn("債権残高").AsDecimal(15, 2).NotNullable().WithDefaultValue(0)
            .WithColumn("債務残高").AsDecimal(15, 2).NotNullable().WithDefaultValue(0)
            .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("作成者名").AsString(100).Nullable()
            .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("更新者名").AsString(100).Nullable()
        |> ignore

        this.Create.ForeignKey("fk_credit_balance_company")
            .FromTable("与信残高データ").ForeignColumn("取引先コード")
            .ToTable("取引先マスタ").PrimaryColumn("取引先コード")
        |> ignore

        this.Create.Index("idx_与信残高データ_受注残高")
            .OnTable("与信残高データ")
            .OnColumn("受注残高").Ascending()
        |> ignore

        this.Create.Index("idx_与信残高データ_債権残高")
            .OnTable("与信残高データ")
            .OnColumn("債権残高").Ascending()
        |> ignore

        this.Execute.Sql("COMMENT ON TABLE 与信残高データ IS '与信残高データ'") |> ignore
        this.Execute.Sql("COMMENT ON COLUMN 与信残高データ.受注残高 IS '受注したが未出荷の金額'") |> ignore
        this.Execute.Sql("COMMENT ON COLUMN 与信残高データ.債権残高 IS '出荷したが未入金の金額'") |> ignore
        this.Execute.Sql("COMMENT ON COLUMN 与信残高データ.債務残高 IS '仕入したが未支払の金額'") |> ignore

    override this.Down() =
        this.Delete.Table("与信残高データ") |> ignore
