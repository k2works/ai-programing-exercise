namespace AccountingSystem.Infrastructure.Migrations

open FluentMigrator

/// <summary>
/// 仕訳エントリテーブルと仕訳明細テーブルを作成
/// </summary>
[<Migration(20250121006L)>]
type Migration_20250121_006_CreateJournalEntry() =
    inherit Migration()

    override this.Up() =
        // 仕訳エントリテーブルを作成
        this.Create.Table("仕訳エントリ")
            .WithColumn("伝票番号").AsString(10).PrimaryKey()
            .WithColumn("仕訳日").AsDate().NotNullable()
            .WithColumn("摘要").AsString(100).NotNullable()
            .WithColumn("合計金額").AsDecimal(15, 2).NotNullable()
            .WithColumn("参照番号").AsString(20).Nullable()
            .WithColumn("作成者").AsString(20).NotNullable()
            .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("更新者").AsString(20).Nullable()
            .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            |> ignore

        // 仕訳明細テーブルを作成
        this.Create.Table("仕訳明細")
            .WithColumn("伝票番号").AsString(10).NotNullable()
            .WithColumn("行番号").AsInt32().NotNullable()
            .WithColumn("勘定科目コード").AsString(20).NotNullable()
            .WithColumn("借方金額").AsDecimal(15, 2).NotNullable().WithDefaultValue(0)
            .WithColumn("貸方金額").AsDecimal(15, 2).NotNullable().WithDefaultValue(0)
            .WithColumn("摘要").AsString(100).NotNullable()
            .WithColumn("消費税額").AsDecimal(15, 2).Nullable().WithDefaultValue(0)
            .WithColumn("消費税率").AsDecimal(5, 2).Nullable()
            |> ignore

        // 仕訳明細の複合主キー
        this.Create.PrimaryKey("PK_仕訳明細")
            .OnTable("仕訳明細")
            .Columns("伝票番号", "行番号")
            |> ignore

        // 外部キー制約
        this.Create.ForeignKey("FK_仕訳明細_仕訳エントリ")
            .FromTable("仕訳明細").ForeignColumn("伝票番号")
            .ToTable("仕訳エントリ").PrimaryColumn("伝票番号")
            .OnDelete(System.Data.Rule.Cascade)
            |> ignore

        this.Create.ForeignKey("FK_仕訳明細_勘定科目マスタ")
            .FromTable("仕訳明細").ForeignColumn("勘定科目コード")
            .ToTable("勘定科目マスタ").PrimaryColumn("勘定科目コード")
            |> ignore

        // インデックスを作成
        this.Create.Index("idx_journal_entry_date")
            .OnTable("仕訳エントリ")
            .OnColumn("仕訳日").Ascending()
            |> ignore

        this.Create.Index("idx_journal_detail_account")
            .OnTable("仕訳明細")
            .OnColumn("勘定科目コード").Ascending()
            |> ignore

    override this.Down() =
        this.Delete.Table("仕訳明細") |> ignore
        this.Delete.Table("仕訳エントリ") |> ignore
