namespace SalesManagement.Infrastructure.Migrations

open FluentMigrator

[<Migration(20250106005L)>]
type Migration_20250106_005_CreateProductTable() =
    inherit Migration()

    override this.Up() =
        this.Create.Table("商品マスタ")
            .WithColumn("商品コード").AsString(20).NotNullable().PrimaryKey()
            .WithColumn("商品正式名").AsString(100).NotNullable()
            .WithColumn("商品略称").AsString(50).NotNullable()
            .WithColumn("商品名カナ").AsString(100).Nullable()
            .WithColumn("商品区分").AsString(10).NotNullable()
            .WithColumn("製品型番").AsString(50).Nullable()
            .WithColumn("販売単価").AsInt32().NotNullable().WithDefaultValue(0)
            .WithColumn("仕入単価").AsInt32().NotNullable().WithDefaultValue(0)
            .WithColumn("売上原価").AsInt32().NotNullable().WithDefaultValue(0)
            .WithColumn("税区分").AsInt32().NotNullable().WithDefaultValue(1)
            .WithColumn("商品分類コード").AsString(20).NotNullable()
            .WithColumn("雑区分").AsInt32().NotNullable().WithDefaultValue(0)
            .WithColumn("在庫管理対象区分").AsInt32().NotNullable().WithDefaultValue(1)
            .WithColumn("在庫引当区分").AsInt32().NotNullable().WithDefaultValue(0)
            .WithColumn("仕入先コード").AsString(20).Nullable()
            .WithColumn("仕入先枝番").AsInt32().Nullable()
            .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("作成者名").AsString(50).NotNullable()
            .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("更新者名").AsString(50).NotNullable()
        |> ignore

        this.Create.Index("idx_商品マスタ_商品分類コード")
            .OnTable("商品マスタ")
            .OnColumn("商品分類コード")
        |> ignore

        this.Create.Index("idx_商品マスタ_商品名カナ")
            .OnTable("商品マスタ")
            .OnColumn("商品名カナ")
        |> ignore

        this.Create.ForeignKey("fk_商品マスタ_商品分類マスタ")
            .FromTable("商品マスタ").ForeignColumn("商品分類コード")
            .ToTable("商品分類マスタ").PrimaryColumn("商品分類コード")
        |> ignore

        this.Execute.Sql("COMMENT ON TABLE 商品マスタ IS '商品の基本情報を管理するマスタ'")
        this.Execute.Sql("COMMENT ON COLUMN 商品マスタ.商品コード IS '商品の一意識別子'")
        this.Execute.Sql("COMMENT ON COLUMN 商品マスタ.販売単価 IS '標準販売単価'")
        this.Execute.Sql("COMMENT ON COLUMN 商品マスタ.在庫管理対象区分 IS '在庫管理が必要か（0:不要, 1:必要）'")

    override this.Down() =
        this.Delete.Table("商品マスタ") |> ignore
