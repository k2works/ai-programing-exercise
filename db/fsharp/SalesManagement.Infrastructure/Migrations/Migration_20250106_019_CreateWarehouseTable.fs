namespace SalesManagement.Infrastructure.Migrations

open FluentMigrator

[<Migration(20250106019L)>]
type Migration_20250106_019_CreateWarehouseTable() =
    inherit Migration()

    override this.Up() =
        this.Create.Table("倉庫マスタ")
            .WithColumn("倉庫コード").AsString(10).NotNullable().PrimaryKey()
            .WithColumn("倉庫名").AsString(100).NotNullable()
            .WithColumn("倉庫区分").AsInt32().NotNullable().WithDefaultValue(1)
            .WithColumn("住所").AsString(500).Nullable()
            .WithColumn("電話番号").AsString(20).Nullable()
            .WithColumn("責任者コード").AsString(10).Nullable()
            .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("作成者名").AsString(100).NotNullable()
            .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("更新者名").AsString(100).NotNullable()
        |> ignore

        this.Create.ForeignKey("fk_warehouse_manager")
            .FromTable("倉庫マスタ").ForeignColumn("責任者コード")
            .ToTable("社員マスタ").PrimaryColumn("社員コード")
        |> ignore

        this.Create.Index("idx_倉庫マスタ_倉庫区分")
            .OnTable("倉庫マスタ")
            .OnColumn("倉庫区分").Ascending()
        |> ignore

        this.Execute.Sql("COMMENT ON TABLE 倉庫マスタ IS '倉庫の基本情報を管理するマスタテーブル'")
        this.Execute.Sql("COMMENT ON COLUMN 倉庫マスタ.倉庫区分 IS '1=自社倉庫、2=委託倉庫など'")
        this.Execute.Sql("COMMENT ON COLUMN 倉庫マスタ.責任者コード IS '倉庫の管理責任者（社員マスタへの参照）'")

    override this.Down() =
        this.Delete.Table("倉庫マスタ") |> ignore
