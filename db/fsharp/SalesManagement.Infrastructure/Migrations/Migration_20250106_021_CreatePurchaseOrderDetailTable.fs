namespace SalesManagement.Infrastructure.Migrations

open FluentMigrator

[<Migration(20250106021L)>]
type Migration_20250106_021_CreatePurchaseOrderDetailTable() =
    inherit Migration()

    override this.Up() =
        this.Create.Table("発注データ明細")
            .WithColumn("発注番号").AsString(10).NotNullable()
            .WithColumn("発注行番号").AsInt32().NotNullable()
            .WithColumn("商品コード").AsString(10).NotNullable()
            .WithColumn("商品名").AsString(200).NotNullable()
            .WithColumn("発注数量").AsInt32().NotNullable().WithDefaultValue(0)
            .WithColumn("入荷済数量").AsInt32().NotNullable().WithDefaultValue(0)
            .WithColumn("仕入単価").AsInt32().NotNullable().WithDefaultValue(0)
            .WithColumn("金額").AsInt32().NotNullable().WithDefaultValue(0)
            .WithColumn("消費税").AsInt32().NotNullable().WithDefaultValue(0)
            .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("作成者名").AsString(100).NotNullable()
            .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("更新者名").AsString(100).NotNullable()
        |> ignore

        this.Create.PrimaryKey("pk_発注データ明細")
            .OnTable("発注データ明細")
            .Columns("発注番号", "発注行番号")
        |> ignore

        this.Create.ForeignKey("fk_purchase_order_detail_header")
            .FromTable("発注データ明細").ForeignColumn("発注番号")
            .ToTable("発注データ").PrimaryColumn("発注番号")
        |> ignore

        this.Create.ForeignKey("fk_purchase_order_detail_product")
            .FromTable("発注データ明細").ForeignColumn("商品コード")
            .ToTable("商品マスタ").PrimaryColumn("商品コード")
        |> ignore

        this.Create.Index("idx_発注データ明細_商品")
            .OnTable("発注データ明細")
            .OnColumn("商品コード").Ascending()
        |> ignore

        this.Execute.Sql("COMMENT ON TABLE 発注データ明細 IS '発注の明細情報（発注した商品と数量）'")
        this.Execute.Sql("COMMENT ON COLUMN 発注データ明細.商品名 IS '発注時点の商品名を履歴として保持'")
        this.Execute.Sql("COMMENT ON COLUMN 発注データ明細.入荷済数量 IS '実際に入荷した数量（仕入登録時に更新）'")
        this.Execute.Sql("COMMENT ON COLUMN 発注データ明細.仕入単価 IS '発注時点の仕入単価を履歴として保持'")

    override this.Down() =
        this.Delete.Table("発注データ明細") |> ignore
