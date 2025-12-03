namespace AccountingSystem.Infrastructure.Migrations

open FluentMigrator
open System.Data

/// <summary>
/// 勘定科目マスタに課税取引コードを追加
/// </summary>
[<Migration(20250121004L)>]
type Migration_20250121_004_AddTaxCodeToAccount() =
    inherit Migration()

    override this.Up() =
        // 課税取引コードを追加
        this.Alter.Table("勘定科目マスタ")
            .AddColumn("課税取引コード").AsString(2).Nullable()
        |> ignore

        // 外部キー制約
        this.Create.ForeignKey("fk_account_tax_transaction")
            .FromTable("勘定科目マスタ").ForeignColumn("課税取引コード")
            .ToTable("課税取引マスタ").PrimaryColumn("課税取引コード")
            .OnDelete(Rule.SetNull)
        |> ignore

        // コメント追加
        this.Execute.Sql(@"
            COMMENT ON COLUMN ""勘定科目マスタ"".""課税取引コード"" IS
                '課税取引コード（課税取引マスタへの外部キー）';
        ")

    override this.Down() =
        this.Delete.ForeignKey("fk_account_tax_transaction").OnTable("勘定科目マスタ") |> ignore
        this.Delete.Column("課税取引コード").FromTable("勘定科目マスタ") |> ignore
