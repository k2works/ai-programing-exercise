namespace SalesManagement.Infrastructure.Migrations

open FluentMigrator

[<Migration(20250106014L)>]
type Migration_20250106_014_CreateCompanyCategoryGroupTable() =
    inherit Migration()

    override this.Up() =
        this.Create.Table("取引先分類所属マスタ")
            .WithColumn("分類種別コード").AsString(4).NotNullable()
            .WithColumn("分類コード").AsString(8).NotNullable()
            .WithColumn("取引先コード").AsString(8).NotNullable()
            .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("作成者名").AsString(100).NotNullable()
            .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("更新者名").AsString(100).NotNullable()
        |> ignore

        // 複合主キー
        this.Create.PrimaryKey("pk_取引先分類所属マスタ")
            .OnTable("取引先分類所属マスタ")
            .Columns("分類種別コード", "分類コード", "取引先コード")
        |> ignore

        // 外部キー: (分類種別コード, 分類コード) -> 取引先分類マスタ
        // FluentMigrator では複合外部キーを生 SQL で定義
        this.Execute.Sql("""
            ALTER TABLE 取引先分類所属マスタ
            ADD CONSTRAINT fk_company_category_group_category
            FOREIGN KEY (分類種別コード, 分類コード)
            REFERENCES 取引先分類マスタ (分類種別コード, 分類コード)
        """) |> ignore

        // 外部キー: 取引先コード -> 取引先マスタ
        this.Create.ForeignKey("fk_company_category_group_company")
            .FromTable("取引先分類所属マスタ").ForeignColumn("取引先コード")
            .ToTable("取引先マスタ").PrimaryColumn("取引先コード")
        |> ignore

    override this.Down() =
        this.Delete.Table("取引先分類所属マスタ") |> ignore
