namespace SalesManagement.Infrastructure.Migrations

open FluentMigrator

[<Migration(20250106024L)>]
type Migration_20250106_024_CreatePurchaseDetailTable() =
    inherit Migration()

    override this.Up() =
        this.Create.Table("仕入データ明細")
            .WithColumn("仕入番号").AsString(10).NotNullable()
            .WithColumn("仕入行番号").AsInt32().NotNullable()
            .WithColumn("商品コード").AsString(10).NotNullable()
            .WithColumn("商品名").AsString(200).NotNullable()
            .WithColumn("ロット番号").AsString(20).NotNullable()
            .WithColumn("倉庫コード").AsString(10).NotNullable()
            .WithColumn("仕入数量").AsInt32().NotNullable().WithDefaultValue(0)
            .WithColumn("仕入単価").AsInt32().NotNullable().WithDefaultValue(0)
            .WithColumn("金額").AsInt32().NotNullable().WithDefaultValue(0)
            .WithColumn("消費税").AsInt32().NotNullable().WithDefaultValue(0)
            .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("作成者名").AsString(100).NotNullable()
            .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("更新者名").AsString(100).NotNullable()
        |> ignore

        this.Create.PrimaryKey("pk_仕入データ明細")
            .OnTable("仕入データ明細")
            .Columns("仕入番号", "仕入行番号")
        |> ignore

        this.Create.ForeignKey("fk_purchase_detail_header")
            .FromTable("仕入データ明細").ForeignColumn("仕入番号")
            .ToTable("仕入データ").PrimaryColumn("仕入番号")
        |> ignore

        this.Create.ForeignKey("fk_purchase_detail_product")
            .FromTable("仕入データ明細").ForeignColumn("商品コード")
            .ToTable("商品マスタ").PrimaryColumn("商品コード")
        |> ignore

        this.Create.ForeignKey("fk_purchase_detail_warehouse")
            .FromTable("仕入データ明細").ForeignColumn("倉庫コード")
            .ToTable("倉庫マスタ").PrimaryColumn("倉庫コード")
        |> ignore

        this.Create.Index("idx_仕入データ明細_商品")
            .OnTable("仕入データ明細")
            .OnColumn("商品コード").Ascending()
        |> ignore

        this.Create.Index("idx_仕入データ明細_ロット")
            .OnTable("仕入データ明細")
            .OnColumn("ロット番号").Ascending()
        |> ignore

        this.Execute.Sql("COMMENT ON TABLE 仕入データ明細 IS '仕入の明細情報（仕入れた商品とロット、数量）'")
        this.Execute.Sql("COMMENT ON COLUMN 仕入データ明細.商品名 IS '仕入時点の商品名を履歴として保持'")
        this.Execute.Sql("COMMENT ON COLUMN 仕入データ明細.ロット番号 IS '仕入時に採番されたロット番号'")
        this.Execute.Sql("COMMENT ON COLUMN 仕入データ明細.倉庫コード IS '仕入商品を格納した倉庫'")
        this.Execute.Sql("COMMENT ON COLUMN 仕入データ明細.仕入単価 IS '仕入時点の単価を履歴として保持'")

    override this.Down() =
        this.Delete.Table("仕入データ明細") |> ignore
