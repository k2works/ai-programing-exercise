namespace SalesManagement.Infrastructure.Migrations

open FluentMigrator

[<Migration(20250106015L)>]
type Migration_20250106_015_CreateOrderTable() =
    inherit Migration()

    override this.Up() =
        this.Create.Table("受注データ")
            .WithColumn("受注番号").AsString(10).NotNullable().PrimaryKey()
            .WithColumn("受注日").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("得意先コード").AsString(8).NotNullable()
            .WithColumn("得意先枝番").AsInt32().NotNullable().WithDefaultValue(0)
            .WithColumn("社員コード").AsString(10).NotNullable()
            .WithColumn("納期").AsDateTime().Nullable()
            .WithColumn("受注金額合計").AsInt32().NotNullable().WithDefaultValue(0)
            .WithColumn("消費税合計").AsInt32().NotNullable().WithDefaultValue(0)
            .WithColumn("伝票備考").AsString(1000).Nullable()
            .WithColumn("部門コード").AsString(10).NotNullable()
            .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("作成者名").AsString(100).NotNullable()
            .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("更新者名").AsString(100).NotNullable()
        |> ignore

        this.Create.ForeignKey("fk_order_customer")
            .FromTable("受注データ").ForeignColumns("得意先コード", "得意先枝番")
            .ToTable("顧客マスタ").PrimaryColumns("顧客コード", "顧客枝番")
        |> ignore

        this.Create.ForeignKey("fk_order_employee")
            .FromTable("受注データ").ForeignColumn("社員コード")
            .ToTable("社員マスタ").PrimaryColumn("社員コード")
        |> ignore

        // 部門コードへの外部キーは設定しない（部門マスタは複合主キー）

        this.Create.Index("idx_受注データ_得意先")
            .OnTable("受注データ")
            .OnColumn("得意先コード").Ascending()
            .OnColumn("得意先枝番").Ascending()
        |> ignore

        this.Create.Index("idx_受注データ_受注日")
            .OnTable("受注データ")
            .OnColumn("受注日").Descending()
        |> ignore

        this.Execute.Sql("COMMENT ON TABLE 受注データ IS '顧客からの受注情報を管理するヘッダ'")
        this.Execute.Sql("COMMENT ON COLUMN 受注データ.納期 IS '顧客が希望する納品日'")

    override this.Down() =
        this.Delete.Table("受注データ") |> ignore
