namespace SalesManagement.Infrastructure.Migrations

open FluentMigrator

[<Migration(20250106011L)>]
type Migration_20250106_011_CreateSupplierTable() =
    inherit Migration()

    override this.Up() =
        this.Create.Table("仕入先マスタ")
            .WithColumn("仕入先コード").AsString(8).NotNullable()
            .WithColumn("仕入先枝番").AsInt32().NotNullable()
            .WithColumn("仕入先区分").AsInt32().NotNullable().WithDefaultValue(0)
            .WithColumn("仕入先名").AsString(40).NotNullable()
            .WithColumn("仕入先名カナ").AsString(40).Nullable()
            .WithColumn("自社担当者コード").AsString(10).NotNullable()
            .WithColumn("仕入先締日").AsInt32().NotNullable()
            .WithColumn("仕入先支払月").AsInt32().NotNullable().WithDefaultValue(1)
            .WithColumn("仕入先支払日").AsInt32().Nullable()
            .WithColumn("仕入先支払方法").AsInt32().NotNullable().WithDefaultValue(1)
            .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("作成者名").AsString(100).NotNullable()
            .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("更新者名").AsString(100).NotNullable()
        |> ignore

        this.Create.PrimaryKey("pk_仕入先マスタ")
            .OnTable("仕入先マスタ")
            .Columns("仕入先コード", "仕入先枝番")
        |> ignore

        this.Create.ForeignKey("fk_supplier_company")
            .FromTable("仕入先マスタ").ForeignColumn("仕入先コード")
            .ToTable("取引先マスタ").PrimaryColumn("取引先コード")
        |> ignore

        this.Create.ForeignKey("fk_supplier_employee")
            .FromTable("仕入先マスタ").ForeignColumn("自社担当者コード")
            .ToTable("社員マスタ").PrimaryColumn("社員コード")
        |> ignore

        this.Create.Index("idx_仕入先マスタ_取引先コード")
            .OnTable("仕入先マスタ")
            .OnColumn("仕入先コード")
        |> ignore

        this.Execute.Sql("COMMENT ON TABLE 仕入先マスタ IS '取引先の仕入先としての詳細情報'")
        this.Execute.Sql("COMMENT ON COLUMN 仕入先マスタ.仕入先枝番 IS '同一取引先の複数仕入先を区別する枝番'")

    override this.Down() =
        this.Delete.Table("仕入先マスタ") |> ignore
