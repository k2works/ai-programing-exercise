namespace SalesManagement.Infrastructure.Migrations

open FluentMigrator

[<Migration(20250106013L)>]
type Migration_20250106_013_CreateCompanyCategoryTable() =
    inherit Migration()

    override this.Up() =
        this.Create.Table("取引先分類マスタ")
            .WithColumn("分類種別コード").AsString(4).NotNullable()
            .WithColumn("分類コード").AsString(8).NotNullable()
            .WithColumn("分類名").AsString(40).NotNullable()
            .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("作成者名").AsString(100).NotNullable()
            .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("更新者名").AsString(100).NotNullable()
        |> ignore

        // 複合主キー
        this.Create.PrimaryKey("pk_取引先分類マスタ")
            .OnTable("取引先分類マスタ")
            .Columns("分類種別コード", "分類コード")
        |> ignore

        // 外部キー: 分類種別コード -> 分類種別マスタ
        this.Create.ForeignKey("fk_company_category_category_type")
            .FromTable("取引先分類マスタ").ForeignColumn("分類種別コード")
            .ToTable("分類種別マスタ").PrimaryColumn("分類種別コード")
        |> ignore

    override this.Down() =
        this.Delete.Table("取引先分類マスタ") |> ignore
