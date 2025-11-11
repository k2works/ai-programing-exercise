namespace SalesManagement.Infrastructure.Migrations

open FluentMigrator

[<Migration(20250106004L)>]
type Migration_20250106_004_CreateProductCategoryTable() =
    inherit Migration()

    override this.Up() =
        this.Create.Table("商品分類マスタ")
            .WithColumn("商品分類コード").AsString(20).NotNullable().PrimaryKey()
            .WithColumn("商品分類名").AsString(100).NotNullable()
            .WithColumn("商品分類階層").AsInt32().NotNullable().WithDefaultValue(1)
            .WithColumn("商品分類パス").AsString(500).NotNullable()
            .WithColumn("最下層区分").AsInt16().NotNullable().WithDefaultValue(1)
            .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("作成者名").AsString(50).NotNullable()
            .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("更新者名").AsString(50).NotNullable()
        |> ignore

        this.Create.Index("idx_商品分類マスタ_商品分類階層")
            .OnTable("商品分類マスタ")
            .OnColumn("商品分類階層")
        |> ignore

        this.Create.Index("idx_商品分類マスタ_商品分類パス")
            .OnTable("商品分類マスタ")
            .OnColumn("商品分類パス")
        |> ignore

        this.Execute.Sql("COMMENT ON TABLE 商品分類マスタ IS '商品の階層的な分類を管理するマスタ'")
        this.Execute.Sql("COMMENT ON COLUMN 商品分類マスタ.商品分類コード IS '商品分類の一意識別子'")
        this.Execute.Sql("COMMENT ON COLUMN 商品分類マスタ.商品分類階層 IS '分類の階層レベル（1:最上位, 2:第2階層...）'")
        this.Execute.Sql("COMMENT ON COLUMN 商品分類マスタ.商品分類パス IS '階層パス（例: CAT001/CAT00101/CAT0010101）'")
        this.Execute.Sql("COMMENT ON COLUMN 商品分類マスタ.最下層区分 IS '最下層かどうか（0:中間階層, 1:最下層）'")

    override this.Down() =
        this.Delete.Table("商品分類マスタ") |> ignore
