namespace SalesManagement.Infrastructure.Migrations

open FluentMigrator

/// <summary>
/// 部門マスタテーブルの作成
/// </summary>
[<Migration(20250106002L)>]
type Migration_20250106_002_CreateDepartmentTable() =
    inherit Migration()

    override this.Up() =
        this.Create.Table("部門マスタ")
            .WithColumn("部門コード").AsString(20).NotNullable()
            .WithColumn("開始日").AsDate().NotNullable()
            .WithColumn("終了日").AsDate().NotNullable()
            .WithColumn("部門名").AsString(100).NotNullable()
            .WithColumn("組織階層").AsInt32().NotNullable().WithDefaultValue(1)
            .WithColumn("部門パス").AsString(500).NotNullable()
            .WithColumn("最下層区分").AsInt16().NotNullable().WithDefaultValue(1)
            .WithColumn("伝票入力可否").AsInt16().NotNullable().WithDefaultValue(0)
            .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("作成者名").AsString(50).NotNullable()
            .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("更新者名").AsString(50).NotNullable()
        |> ignore

        this.Create.PrimaryKey("pk_部門マスタ")
            .OnTable("部門マスタ")
            .Columns("部門コード", "開始日")
        |> ignore

        this.Create.Index("idx_部門マスタ_組織階層")
            .OnTable("部門マスタ")
            .OnColumn("組織階層")
        |> ignore

        this.Create.Index("idx_部門マスタ_部門パス")
            .OnTable("部門マスタ")
            .OnColumn("部門パス")
        |> ignore

        // テーブルコメント（PostgreSQL）
        this.Execute.Sql("COMMENT ON TABLE 部門マスタ IS '組織の部門情報を管理するマスタ'")
        this.Execute.Sql("COMMENT ON COLUMN 部門マスタ.部門コード IS '部門の一意識別子'")
        this.Execute.Sql("COMMENT ON COLUMN 部門マスタ.開始日 IS '部門の有効開始日（履歴管理）'")
        this.Execute.Sql("COMMENT ON COLUMN 部門マスタ.終了日 IS '部門の有効終了日'")
        this.Execute.Sql("COMMENT ON COLUMN 部門マスタ.部門名 IS '部門の名称'")
        this.Execute.Sql("COMMENT ON COLUMN 部門マスタ.組織階層 IS '組織内での階層レベル（1:最上位, 2:第2階層...）'")
        this.Execute.Sql("COMMENT ON COLUMN 部門マスタ.部門パス IS '階層パス（例: 10000/11000/11101）'")
        this.Execute.Sql("COMMENT ON COLUMN 部門マスタ.最下層区分 IS '最下層かどうか（0:中間階層, 1:最下層）'")
        this.Execute.Sql("COMMENT ON COLUMN 部門マスタ.伝票入力可否 IS '伝票入力が可能か（0:不可, 1:可）'")

    override this.Down() =
        this.Delete.Table("部門マスタ") |> ignore
