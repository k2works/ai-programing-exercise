namespace AccountingSystem.Infrastructure.Migrations

open FluentMigrator

/// <summary>
/// 自動仕訳テーブルを作成するマイグレーション
/// 日付管理方式による自動仕訳システム
/// </summary>
[<Migration(20250121009L)>]
type Migration_20250121_009_CreateAutoJournal() =
    inherit Migration()

    override this.Up() =
        // 自動仕訳管理テーブル
        this.Create.Table("自動仕訳管理")
            .WithColumn("id").AsInt64().NotNullable().PrimaryKey().Identity()
            .WithColumn("ソーステーブル名").AsString(100).NotNullable().Unique()
            .WithColumn("最終処理日時").AsDateTime().NotNullable()
            .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentUTCDateTime)
            .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentUTCDateTime)
        |> ignore

        // 自動仕訳パターンテーブル
        this.Create.Table("自動仕訳パターン")
            .WithColumn("id").AsInt64().NotNullable().PrimaryKey().Identity()
            .WithColumn("パターンコード").AsString(20).NotNullable().Unique()
            .WithColumn("パターン名").AsString(100).NotNullable()
            .WithColumn("ソーステーブル名").AsString(100).NotNullable()
            .WithColumn("説明").AsString(500).Nullable()
            .WithColumn("有効フラグ").AsBoolean().NotNullable().WithDefaultValue(true)
            .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentUTCDateTime)
            .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentUTCDateTime)
        |> ignore

        // 自動仕訳パターン明細テーブル
        this.Create.Table("自動仕訳パターン明細")
            .WithColumn("id").AsInt64().NotNullable().PrimaryKey().Identity()
            .WithColumn("パターンID").AsInt64().NotNullable().ForeignKey("自動仕訳パターン", "id").OnDeleteOrUpdate(System.Data.Rule.Cascade)
            .WithColumn("行番号").AsInt32().NotNullable()
            .WithColumn("貸借区分").AsString(1).NotNullable()
            .WithColumn("勘定科目コード").AsString(10).NotNullable()
            .WithColumn("金額式").AsString(200).NotNullable()
            .WithColumn("摘要テンプレート").AsString(200).Nullable()
            .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentUTCDateTime)
            .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentUTCDateTime)
        |> ignore

        // 自動仕訳実行ログテーブル
        this.Create.Table("自動仕訳実行ログ")
            .WithColumn("id").AsInt64().NotNullable().PrimaryKey().Identity()
            .WithColumn("パターンID").AsInt64().NotNullable().ForeignKey("自動仕訳パターン", "id")
            .WithColumn("実行日時").AsDateTime().NotNullable()
            .WithColumn("処理件数").AsInt32().NotNullable()
            .WithColumn("生成件数").AsInt32().NotNullable()
            .WithColumn("ステータス").AsString(20).NotNullable()
            .WithColumn("メッセージ").AsString(500).Nullable()
            .WithColumn("エラー詳細").AsCustom("TEXT").Nullable()
            .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentUTCDateTime)
        |> ignore

        // インデックス
        this.Create.Index("idx_自動仕訳パターン_ソーステーブル")
            .OnTable("自動仕訳パターン")
            .OnColumn("ソーステーブル名").Ascending()
        |> ignore

        this.Create.Index("idx_自動仕訳パターン明細_パターンID")
            .OnTable("自動仕訳パターン明細")
            .OnColumn("パターンID").Ascending()
        |> ignore

        this.Create.Index("idx_自動仕訳実行ログ_パターンID")
            .OnTable("自動仕訳実行ログ")
            .OnColumn("パターンID").Ascending()
        |> ignore

        this.Create.Index("idx_自動仕訳実行ログ_実行日時")
            .OnTable("自動仕訳実行ログ")
            .OnColumn("実行日時").Descending()
        |> ignore

        // CHECK制約
        this.Execute.Sql("""
            ALTER TABLE "自動仕訳パターン明細"
              ADD CONSTRAINT check_pattern_debit_credit
              CHECK ("貸借区分" IN ('D', 'C'))
        """) |> ignore

        this.Execute.Sql("""
            ALTER TABLE "自動仕訳実行ログ"
              ADD CONSTRAINT check_status
              CHECK ("ステータス" IN ('SUCCESS', 'FAILURE', 'PARTIAL'))
        """) |> ignore

    override this.Down() =
        this.Delete.Table("自動仕訳実行ログ") |> ignore
        this.Delete.Table("自動仕訳パターン明細") |> ignore
        this.Delete.Table("自動仕訳パターン") |> ignore
        this.Delete.Table("自動仕訳管理") |> ignore
