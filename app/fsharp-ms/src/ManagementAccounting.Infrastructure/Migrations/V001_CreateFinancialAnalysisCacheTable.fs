namespace ManagementAccounting.Infrastructure.Migrations

open FluentMigrator

/// <summary>
/// 財務分析キャッシュテーブルの作成
/// </summary>
[<Migration(1L)>]
type V001_CreateFinancialAnalysisCacheTable() =
    inherit Migration()

    override this.Up() =
        this.Create.Table("financial_analysis_cache")
            .WithColumn("id").AsInt32().PrimaryKey().Identity()
            .WithColumn("fiscal_year").AsInt32().NotNullable().Unique()
            .WithColumn("data_json").AsString(int System.Int16.MaxValue).NotNullable()
            .WithColumn("ratios_json").AsString(int System.Int16.MaxValue).NotNullable()
            .WithColumn("cached_at").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentUTCDateTime)
        |> ignore

        this.Create.Index("ix_financial_analysis_cache_fiscal_year")
            .OnTable("financial_analysis_cache")
            .OnColumn("fiscal_year")
        |> ignore

    override this.Down() =
        this.Delete.Table("financial_analysis_cache") |> ignore
