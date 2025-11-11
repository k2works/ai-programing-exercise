namespace SalesManagement.Infrastructure.Migrations

open FluentMigrator

[<Migration(20250106007L)>]
type Migration_20250106_007_CreateAlternateProductTable() =
    inherit Migration()

    override this.Up() =
        this.Create.Table("代替商品")
            .WithColumn("商品コード").AsString(20).NotNullable()
            .WithColumn("代替商品コード").AsString(20).NotNullable()
            .WithColumn("優先順位").AsInt32().NotNullable().WithDefaultValue(1)
            .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("作成者名").AsString(50).NotNullable()
            .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("更新者名").AsString(50).NotNullable()
        |> ignore

        this.Create.PrimaryKey("pk_代替商品")
            .OnTable("代替商品")
            .Columns("商品コード", "代替商品コード")
        |> ignore

        this.Create.Index("idx_代替商品_優先順位")
            .OnTable("代替商品")
            .OnColumn("優先順位")
        |> ignore

        this.Create.ForeignKey("fk_代替商品_商品マスタ_商品コード")
            .FromTable("代替商品").ForeignColumn("商品コード")
            .ToTable("商品マスタ").PrimaryColumn("商品コード")
        |> ignore

        this.Create.ForeignKey("fk_代替商品_商品マスタ_代替商品コード")
            .FromTable("代替商品").ForeignColumn("代替商品コード")
            .ToTable("商品マスタ").PrimaryColumn("商品コード")
        |> ignore

        this.Execute.Sql("COMMENT ON TABLE 代替商品 IS '在庫切れ時などに提案する代替商品を管理'")
        this.Execute.Sql("COMMENT ON COLUMN 代替商品.優先順位 IS '複数の代替商品がある場合の優先順位'")

    override this.Down() =
        this.Delete.Table("代替商品") |> ignore
