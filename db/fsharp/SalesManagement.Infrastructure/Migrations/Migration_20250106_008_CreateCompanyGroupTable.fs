namespace SalesManagement.Infrastructure.Migrations

open FluentMigrator

[<Migration(20250106008L)>]
type Migration_20250106_008_CreateCompanyGroupTable() =
    inherit Migration()

    override this.Up() =
        this.Create.Table("取引先グループマスタ")
            .WithColumn("取引先グループコード").AsString(4).NotNullable().PrimaryKey()
            .WithColumn("取引先グループ名").AsString(40).NotNullable()
            .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("作成者名").AsString(100).NotNullable()
            .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("更新者名").AsString(100).NotNullable()
        |> ignore

        this.Execute.Sql("COMMENT ON TABLE 取引先グループマスタ IS '取引先をグループ化するマスタ'")

    override this.Down() =
        this.Delete.Table("取引先グループマスタ") |> ignore
