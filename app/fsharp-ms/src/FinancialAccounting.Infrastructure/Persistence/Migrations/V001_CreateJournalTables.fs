namespace FinancialAccounting.Infrastructure.Persistence.Migrations

open FluentMigrator

/// <summary>
/// 仕訳テーブルの作成
/// </summary>
[<Migration(1L)>]
type V001_CreateJournalTables() =
    inherit Migration()

    override this.Up() =
        // 勘定科目マスタ
        this.Create.Table("accounts")
            .WithColumn("account_id").AsInt32().PrimaryKey().Identity()
            .WithColumn("account_code").AsString(20).NotNullable().Unique()
            .WithColumn("account_name").AsString(100).NotNullable()
            .WithColumn("account_name_kana").AsString(100).Nullable()
            .WithColumn("account_type").AsString(20).NotNullable()
            .WithColumn("is_summary_account").AsBoolean().NotNullable().WithDefaultValue(false)
            .WithColumn("bs_pl_type").AsString(1).NotNullable()
            .WithColumn("transaction_element_type").AsString(10).NotNullable()
            .WithColumn("expense_type").AsString(20).Nullable()
            .WithColumn("display_order").AsInt32().NotNullable().WithDefaultValue(0)
            .WithColumn("is_aggregation_target").AsBoolean().NotNullable().WithDefaultValue(true)
            .WithColumn("tax_code").AsString(10).Nullable()
            .WithColumn("balance").AsDecimal(15, 2).NotNullable().WithDefaultValue(0)
            .WithColumn("created_at").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentUTCDateTime)
            .WithColumn("updated_at").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentUTCDateTime)
        |> ignore

        // 仕訳ヘッダー
        this.Create.Table("journals")
            .WithColumn("journal_id").AsInt32().PrimaryKey().Identity()
            .WithColumn("journal_date").AsDate().NotNullable()
            .WithColumn("description").AsString(500).NotNullable()
            .WithColumn("fiscal_year").AsInt32().NotNullable()
            .WithColumn("created_at").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentUTCDateTime)
            .WithColumn("updated_at").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentUTCDateTime)
        |> ignore

        this.Create.Index("idx_journals_fiscal_year").OnTable("journals").OnColumn("fiscal_year") |> ignore
        this.Create.Index("idx_journals_journal_date").OnTable("journals").OnColumn("journal_date") |> ignore

        // 仕訳明細
        this.Create.Table("journal_entries")
            .WithColumn("entry_id").AsInt32().PrimaryKey().Identity()
            .WithColumn("journal_id").AsInt32().NotNullable()
            .WithColumn("account_code").AsString(20).NotNullable()
            .WithColumn("debit_amount").AsDecimal(15, 2).NotNullable().WithDefaultValue(0)
            .WithColumn("credit_amount").AsDecimal(15, 2).NotNullable().WithDefaultValue(0)
            .WithColumn("description").AsString(500).Nullable()
        |> ignore

        this.Create.ForeignKey("fk_journal_entries_journals")
            .FromTable("journal_entries").ForeignColumn("journal_id")
            .ToTable("journals").PrimaryColumn("journal_id")
            .OnDelete(System.Data.Rule.Cascade)
        |> ignore

        this.Create.Index("idx_journal_entries_journal_id").OnTable("journal_entries").OnColumn("journal_id") |> ignore
        this.Create.Index("idx_journal_entries_account_code").OnTable("journal_entries").OnColumn("account_code") |> ignore

    override this.Down() =
        this.Delete.Table("journal_entries") |> ignore
        this.Delete.Table("journals") |> ignore
        this.Delete.Table("accounts") |> ignore
