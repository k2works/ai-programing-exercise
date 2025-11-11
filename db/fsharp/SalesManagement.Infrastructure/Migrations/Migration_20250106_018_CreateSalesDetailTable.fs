namespace SalesManagement.Infrastructure.Migrations

open System.Data
open FluentMigrator

[<Migration(20250106018L)>]
type Migration_20250106_018_CreateSalesDetailTable() =
    inherit Migration()

    override this.Up() =
        this.Create.Table("売上データ明細")
            .WithColumn("売上伝票番号").AsString(10).NotNullable()
            .WithColumn("売上行番号").AsInt32().NotNullable()
            .WithColumn("商品コード").AsString(20).NotNullable()
            .WithColumn("商品名").AsString(200).NotNullable()
            .WithColumn("売上数量").AsInt32().NotNullable().WithDefaultValue(0)
            .WithColumn("売単価").AsInt32().NotNullable().WithDefaultValue(0)
            .WithColumn("金額").AsInt32().NotNullable().WithDefaultValue(0)
            .WithColumn("消費税").AsInt32().NotNullable().WithDefaultValue(0)
            .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("作成者名").AsString(100).NotNullable()
            .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("更新者名").AsString(100).NotNullable()
        |> ignore

        this.Create.PrimaryKey("pk_売上データ明細")
            .OnTable("売上データ明細")
            .Columns("売上伝票番号", "売上行番号")
        |> ignore

        this.Create.ForeignKey("fk_sales_detail_sales")
            .FromTable("売上データ明細").ForeignColumn("売上伝票番号")
            .ToTable("売上データ").PrimaryColumn("売上伝票番号")
            .OnDelete(Rule.Cascade)
        |> ignore

        this.Create.ForeignKey("fk_sales_detail_product")
            .FromTable("売上データ明細").ForeignColumn("商品コード")
            .ToTable("商品マスタ").PrimaryColumn("商品コード")
        |> ignore

        this.Create.Index("idx_売上データ明細_商品")
            .OnTable("売上データ明細")
            .OnColumn("商品コード")
        |> ignore

        this.Execute.Sql("COMMENT ON TABLE 売上データ明細 IS '売上データの明細行'")
        this.Execute.Sql("COMMENT ON COLUMN 売上データ明細.商品名 IS 'マスタ変更の影響を受けないよう履歴保持'")
        this.Execute.Sql("COMMENT ON COLUMN 売上データ明細.売単価 IS 'マスタ変更の影響を受けないよう履歴保持'")

    override this.Down() =
        this.Delete.Table("売上データ明細") |> ignore
