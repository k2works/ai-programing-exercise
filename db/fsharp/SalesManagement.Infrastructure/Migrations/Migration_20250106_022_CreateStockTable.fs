namespace SalesManagement.Infrastructure.Migrations

open FluentMigrator

[<Migration(20250106022L)>]
type Migration_20250106_022_CreateStockTable() =
    inherit Migration()

    override this.Up() =
        this.Create.Table("在庫データ")
            .WithColumn("倉庫コード").AsString(10).NotNullable()
            .WithColumn("商品コード").AsString(10).NotNullable()
            .WithColumn("ロット番号").AsString(20).NotNullable()
            .WithColumn("在庫区分").AsString(1).NotNullable()
            .WithColumn("良品区分").AsString(1).NotNullable()
            .WithColumn("実在庫数").AsInt32().NotNullable().WithDefaultValue(0)
            .WithColumn("有効在庫数").AsInt32().NotNullable().WithDefaultValue(0)
            .WithColumn("最終出荷日").AsDateTime().Nullable()
            .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("作成者名").AsString(100).NotNullable()
            .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("更新者名").AsString(100).NotNullable()
        |> ignore

        this.Create.PrimaryKey("pk_在庫データ")
            .OnTable("在庫データ")
            .Columns("倉庫コード", "商品コード", "ロット番号", "在庫区分", "良品区分")
        |> ignore

        this.Create.ForeignKey("fk_stock_warehouse")
            .FromTable("在庫データ").ForeignColumn("倉庫コード")
            .ToTable("倉庫マスタ").PrimaryColumn("倉庫コード")
        |> ignore

        this.Create.ForeignKey("fk_stock_product")
            .FromTable("在庫データ").ForeignColumn("商品コード")
            .ToTable("商品マスタ").PrimaryColumn("商品コード")
        |> ignore

        this.Create.Index("idx_在庫データ_倉庫")
            .OnTable("在庫データ")
            .OnColumn("倉庫コード").Ascending()
        |> ignore

        this.Create.Index("idx_在庫データ_商品")
            .OnTable("在庫データ")
            .OnColumn("商品コード").Ascending()
        |> ignore

        this.Create.Index("idx_在庫データ_ロット")
            .OnTable("在庫データ")
            .OnColumn("ロット番号").Ascending()
        |> ignore

        this.Execute.Sql("COMMENT ON TABLE 在庫データ IS 'ロット単位で在庫を管理するテーブル'")
        this.Execute.Sql("COMMENT ON COLUMN 在庫データ.在庫区分 IS '1=通常在庫、2=預託在庫など'")
        this.Execute.Sql("COMMENT ON COLUMN 在庫データ.良品区分 IS 'G=良品、B=不良品、H=保留品'")
        this.Execute.Sql("COMMENT ON COLUMN 在庫データ.実在庫数 IS '物理的な在庫数量'")
        this.Execute.Sql("COMMENT ON COLUMN 在庫データ.有効在庫数 IS '引当可能な在庫数量（実在庫 - 引当済数量）'")

    override this.Down() =
        this.Delete.Table("在庫データ") |> ignore
