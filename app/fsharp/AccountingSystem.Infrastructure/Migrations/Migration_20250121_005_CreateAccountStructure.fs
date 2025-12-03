namespace AccountingSystem.Infrastructure.Migrations

open FluentMigrator
open System.Data

/// <summary>
/// 勘定科目構成マスタの作成
/// </summary>
[<Migration(20250121005L)>]
type Migration_20250121_005_CreateAccountStructure() =
    inherit Migration()

    override this.Up() =
        // 勘定科目構成マスタ
        this.Create.Table("勘定科目構成マスタ")
            .WithColumn("勘定科目コード").AsString(20).PrimaryKey()
            .WithColumn("勘定科目パス").AsString(200).NotNullable()
            .WithColumn("階層レベル").AsInt32().NotNullable().WithDefaultValue(1)
            .WithColumn("親科目コード").AsString(20).Nullable()
            .WithColumn("表示順序").AsInt32().NotNullable().WithDefaultValue(0)
            .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
        |> ignore

        // 外部キー制約（勘定科目コード）
        this.Create.ForeignKey("fk_account_structure_account")
            .FromTable("勘定科目構成マスタ").ForeignColumn("勘定科目コード")
            .ToTable("勘定科目マスタ").PrimaryColumn("勘定科目コード")
            .OnDelete(Rule.Cascade)
        |> ignore

        // コメント追加
        this.Execute.Sql(@"
            COMMENT ON TABLE ""勘定科目構成マスタ"" IS '勘定科目の階層構造を管理するマスタテーブル';
            COMMENT ON COLUMN ""勘定科目構成マスタ"".""勘定科目コード"" IS '勘定科目コード';
            COMMENT ON COLUMN ""勘定科目構成マスタ"".""勘定科目パス"" IS 'チルダ連結形式のパス（例: 11~11000~11190~11110）';
            COMMENT ON COLUMN ""勘定科目構成マスタ"".""階層レベル"" IS '階層の深さ（ルート=1）';
            COMMENT ON COLUMN ""勘定科目構成マスタ"".""親科目コード"" IS '親科目のコード';
            COMMENT ON COLUMN ""勘定科目構成マスタ"".""表示順序"" IS '同じ階層内での表示順序';
        ")

        // パスでの検索を高速化するためのインデックス
        this.Create.Index("idx_account_structure_path")
            .OnTable("勘定科目構成マスタ")
            .OnColumn("勘定科目パス")
        |> ignore

        // 親科目での検索を高速化するためのインデックス
        this.Execute.Sql(@"
            CREATE INDEX IF NOT EXISTS idx_account_structure_parent
                ON ""勘定科目構成マスタ"" (""親科目コード"")
                WHERE ""親科目コード"" IS NOT NULL;
        ")

        // 階層レベルでの検索を高速化するためのインデックス
        this.Create.Index("idx_account_structure_level")
            .OnTable("勘定科目構成マスタ")
            .OnColumn("階層レベル")
        |> ignore

    override this.Down() =
        this.Delete.Table("勘定科目構成マスタ") |> ignore
