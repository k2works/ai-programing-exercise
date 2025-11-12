namespace SalesManagement.Infrastructure.Migrations

open FluentMigrator

[<Migration(20250106023L)>]
type Migration_20250106_023_CreatePurchaseTable() =
    inherit Migration()

    override this.Up() =
        this.Create.Table("仕入データ")
            .WithColumn("仕入番号").AsString(10).NotNullable().PrimaryKey()
            .WithColumn("仕入日").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("発注番号").AsString(10).NotNullable()
            .WithColumn("仕入先コード").AsString(8).NotNullable()
            .WithColumn("仕入先枝番").AsInt32().NotNullable().WithDefaultValue(0)
            .WithColumn("社員コード").AsString(10).NotNullable()
            .WithColumn("仕入金額合計").AsInt32().NotNullable().WithDefaultValue(0)
            .WithColumn("消費税合計").AsInt32().NotNullable().WithDefaultValue(0)
            .WithColumn("備考").AsString(1000).Nullable()
            .WithColumn("部門コード").AsString(10).NotNullable()
            .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("作成者名").AsString(100).NotNullable()
            .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("更新者名").AsString(100).NotNullable()
        |> ignore

        this.Create.ForeignKey("fk_purchase_purchase_order")
            .FromTable("仕入データ").ForeignColumn("発注番号")
            .ToTable("発注データ").PrimaryColumn("発注番号")
        |> ignore

        this.Create.ForeignKey("fk_purchase_supplier")
            .FromTable("仕入データ").ForeignColumns("仕入先コード", "仕入先枝番")
            .ToTable("仕入先マスタ").PrimaryColumns("仕入先コード", "仕入先枝番")
        |> ignore

        this.Create.ForeignKey("fk_purchase_employee")
            .FromTable("仕入データ").ForeignColumn("社員コード")
            .ToTable("社員マスタ").PrimaryColumn("社員コード")
        |> ignore

        // 部門コードへの外部キーは設定しない（部門マスタは複合主キー）

        this.Create.Index("idx_仕入データ_発注番号")
            .OnTable("仕入データ")
            .OnColumn("発注番号").Ascending()
        |> ignore

        this.Create.Index("idx_仕入データ_仕入先")
            .OnTable("仕入データ")
            .OnColumn("仕入先コード").Ascending()
            .OnColumn("仕入先枝番").Ascending()
        |> ignore

        this.Create.Index("idx_仕入データ_仕入日")
            .OnTable("仕入データ")
            .OnColumn("仕入日").Descending()
        |> ignore

        this.Execute.Sql("COMMENT ON TABLE 仕入データ IS '仕入先からの仕入情報を管理するヘッダテーブル'")
        this.Execute.Sql("COMMENT ON COLUMN 仕入データ.発注番号 IS 'トレーサビリティのため元の発注番号を記録'")

    override this.Down() =
        this.Delete.Table("仕入データ") |> ignore
