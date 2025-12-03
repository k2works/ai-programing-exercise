namespace AccountingSystem.Infrastructure.Migrations

open FluentMigrator

/// <summary>
/// 3層構造の仕訳テーブルを作成するマイグレーション
/// 仕訳 + 仕訳明細 + 仕訳貸借明細
/// </summary>
[<Migration(20250121007L)>]
type Migration_20250121_007_Create3LayerJournal() =
    inherit Migration()

    override this.Up() =
        // 仕訳テーブル（ヘッダー）
        this.Create.Table("仕訳")
            .WithColumn("仕訳伝票番号").AsString(10).NotNullable().PrimaryKey()
            .WithColumn("起票日").AsDate().NotNullable()
            .WithColumn("入力日").AsDate().NotNullable()
            .WithColumn("決算仕訳フラグ").AsInt16().NotNullable().WithDefaultValue(0)
            .WithColumn("単振フラグ").AsInt16().NotNullable().WithDefaultValue(0)
            .WithColumn("仕訳伝票区分").AsInt16().NotNullable()
            .WithColumn("定期計上フラグ").AsInt16().NotNullable().WithDefaultValue(0)
            .WithColumn("社員コード").AsString(10).Nullable()
            .WithColumn("部門コード").AsString(5).Nullable()
            .WithColumn("赤伝フラグ").AsInt16().NotNullable().WithDefaultValue(0)
            .WithColumn("赤黒伝票番号").AsInt32().Nullable()
            .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentUTCDateTime)
            .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentUTCDateTime)
        |> ignore

        // 仕訳明細テーブル（2層目）
        this.Create.Table("仕訳明細V2")
            .WithColumn("仕訳伝票番号").AsString(10).NotNullable().PrimaryKey()
            .WithColumn("仕訳行番号").AsInt16().NotNullable().PrimaryKey()
            .WithColumn("行摘要").AsString(1000).NotNullable()
            .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentUTCDateTime)
            .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentUTCDateTime)
        |> ignore

        // 仕訳貸借明細テーブル（3層目）
        this.Create.Table("仕訳貸借明細")
            .WithColumn("仕訳伝票番号").AsString(10).NotNullable().PrimaryKey()
            .WithColumn("仕訳行番号").AsInt16().NotNullable().PrimaryKey()
            .WithColumn("仕訳行貸借区分").AsString(1).NotNullable().PrimaryKey()
            .WithColumn("通貨コード").AsString(3).NotNullable().WithDefaultValue("JPY")
            .WithColumn("為替レート").AsDecimal(8, 2).NotNullable().WithDefaultValue(1.0)
            .WithColumn("部門コード").AsString(3).Nullable()
            .WithColumn("プロジェクトコード").AsString(10).Nullable()
            .WithColumn("勘定科目コード").AsString(5).NotNullable()
            .WithColumn("補助科目コード").AsString(10).Nullable()
            .WithColumn("仕訳金額").AsDecimal(14, 2).NotNullable()
            .WithColumn("基軸換算仕訳金額").AsDecimal(14, 2).NotNullable()
            .WithColumn("消費税区分").AsString(2).Nullable()
            .WithColumn("消費税率").AsInt16().Nullable()
            .WithColumn("消費税計算区分").AsString(2).Nullable()
            .WithColumn("期日").AsDate().Nullable()
            .WithColumn("資金繰フラグ").AsInt16().NotNullable().WithDefaultValue(0)
            .WithColumn("セグメントコード").AsString(10).Nullable()
            .WithColumn("相手勘定科目コード").AsString(5).Nullable()
            .WithColumn("相手補助科目コード").AsString(10).Nullable()
            .WithColumn("付箋コード").AsString(1).Nullable()
            .WithColumn("付箋内容").AsString(60).Nullable()
            .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentUTCDateTime)
            .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentUTCDateTime)
        |> ignore

        // 外部キー制約
        this.Create.ForeignKey("fk_仕訳明細V2_仕訳")
            .FromTable("仕訳明細V2").ForeignColumn("仕訳伝票番号")
            .ToTable("仕訳").PrimaryColumn("仕訳伝票番号")
            .OnDeleteOrUpdate(System.Data.Rule.Cascade)
        |> ignore

        this.Create.ForeignKey("fk_仕訳貸借明細_仕訳明細V2")
            .FromTable("仕訳貸借明細").ForeignColumns("仕訳伝票番号", "仕訳行番号")
            .ToTable("仕訳明細V2").PrimaryColumns("仕訳伝票番号", "仕訳行番号")
            .OnDeleteOrUpdate(System.Data.Rule.Cascade)
        |> ignore

        this.Create.ForeignKey("fk_仕訳貸借明細_勘定科目")
            .FromTable("仕訳貸借明細").ForeignColumn("勘定科目コード")
            .ToTable("勘定科目マスタ").PrimaryColumn("勘定科目コード")
        |> ignore

        // インデックス
        this.Create.Index("idx_仕訳_起票日")
            .OnTable("仕訳")
            .OnColumn("起票日").Ascending()
        |> ignore

        this.Create.Index("idx_仕訳_部門コード")
            .OnTable("仕訳")
            .OnColumn("部門コード").Ascending()
        |> ignore

        this.Create.Index("idx_仕訳_赤伝フラグ")
            .OnTable("仕訳")
            .OnColumn("赤伝フラグ").Ascending()
        |> ignore

        this.Create.Index("idx_仕訳貸借明細_勘定科目コード")
            .OnTable("仕訳貸借明細")
            .OnColumn("勘定科目コード").Ascending()
        |> ignore

        this.Create.Index("idx_仕訳貸借明細_部門コード")
            .OnTable("仕訳貸借明細")
            .OnColumn("部門コード").Ascending()
        |> ignore

        this.Create.Index("idx_仕訳貸借明細_プロジェクトコード")
            .OnTable("仕訳貸借明細")
            .OnColumn("プロジェクトコード").Ascending()
        |> ignore

    override this.Down() =
        this.Delete.ForeignKey("fk_仕訳貸借明細_勘定科目").OnTable("仕訳貸借明細") |> ignore
        this.Delete.ForeignKey("fk_仕訳貸借明細_仕訳明細V2").OnTable("仕訳貸借明細") |> ignore
        this.Delete.ForeignKey("fk_仕訳明細V2_仕訳").OnTable("仕訳明細V2") |> ignore
        this.Delete.Table("仕訳貸借明細") |> ignore
        this.Delete.Table("仕訳明細V2") |> ignore
        this.Delete.Table("仕訳") |> ignore
