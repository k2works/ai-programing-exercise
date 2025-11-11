namespace SalesManagement.Infrastructure.Migrations

open System.Data
open FluentMigrator

[<Migration(20250106016L)>]
type Migration_20250106_016_CreateOrderDetailTable() =
    inherit Migration()

    override this.Up() =
        this.Create.Table("受注データ明細")
            .WithColumn("受注番号").AsString(10).NotNullable()
            .WithColumn("受注行番号").AsInt32().NotNullable()
            .WithColumn("商品コード").AsString(20).NotNullable()
            .WithColumn("商品名").AsString(200).NotNullable()
            .WithColumn("受注数量").AsInt32().NotNullable().WithDefaultValue(0)
            .WithColumn("引当数量").AsInt32().NotNullable().WithDefaultValue(0)
            .WithColumn("出荷指示数量").AsInt32().NotNullable().WithDefaultValue(0)
            .WithColumn("出荷済数量").AsInt32().NotNullable().WithDefaultValue(0)
            .WithColumn("売単価").AsInt32().NotNullable().WithDefaultValue(0)
            .WithColumn("金額").AsInt32().NotNullable().WithDefaultValue(0)
            .WithColumn("消費税").AsInt32().NotNullable().WithDefaultValue(0)
            .WithColumn("完了フラグ").AsInt32().NotNullable().WithDefaultValue(0)
            .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("作成者名").AsString(100).NotNullable()
            .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("更新者名").AsString(100).NotNullable()
        |> ignore

        this.Create.PrimaryKey("pk_受注データ明細")
            .OnTable("受注データ明細")
            .Columns("受注番号", "受注行番号")
        |> ignore

        this.Create.ForeignKey("fk_order_detail_order")
            .FromTable("受注データ明細").ForeignColumn("受注番号")
            .ToTable("受注データ").PrimaryColumn("受注番号")
            .OnDelete(Rule.Cascade)
        |> ignore

        this.Create.ForeignKey("fk_order_detail_product")
            .FromTable("受注データ明細").ForeignColumn("商品コード")
            .ToTable("商品マスタ").PrimaryColumn("商品コード")
        |> ignore

        this.Create.Index("idx_受注データ明細_商品")
            .OnTable("受注データ明細")
            .OnColumn("商品コード")
        |> ignore

        this.Execute.Sql("COMMENT ON TABLE 受注データ明細 IS '受注データの明細行'")
        this.Execute.Sql("COMMENT ON COLUMN 受注データ明細.商品名 IS 'マスタ変更の影響を受けないよう履歴保持'")
        this.Execute.Sql("COMMENT ON COLUMN 受注データ明細.売単価 IS 'マスタ変更の影響を受けないよう履歴保持'")
        this.Execute.Sql("COMMENT ON COLUMN 受注データ明細.完了フラグ IS '0=未完了、1=出荷完了'")

    override this.Down() =
        this.Delete.Table("受注データ明細") |> ignore
