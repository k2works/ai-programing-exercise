namespace SalesManagement.Infrastructure.Migrations

open FluentMigrator

[<Migration(20250106012L)>]
type Migration_20250106_012_CreateCategoryTypeTable() =
    inherit Migration()

    override this.Up() =
        this.Create.Table("分類種別マスタ")
            .WithColumn("分類種別コード").AsString(4).NotNullable().PrimaryKey()
            .WithColumn("分類種別名").AsString(40).NotNullable()
            .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("作成者名").AsString(100).NotNullable()
            .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("更新者名").AsString(100).NotNullable()
        |> ignore

    override this.Down() =
        this.Delete.Table("分類種別マスタ") |> ignore
