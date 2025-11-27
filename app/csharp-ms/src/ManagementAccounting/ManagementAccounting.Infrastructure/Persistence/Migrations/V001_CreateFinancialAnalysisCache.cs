using FluentMigrator;

namespace ManagementAccounting.Infrastructure.Persistence.Migrations;

/// <summary>
/// 財務分析キャッシュテーブルの作成
/// </summary>
[Migration(1)]
public class V001_CreateFinancialAnalysisCache : Migration
{
    public override void Up()
    {
        Create.Table("financial_analysis_cache")
            .WithColumn("id").AsInt32().PrimaryKey().Identity()
            .WithColumn("fiscal_year").AsInt32().Unique().NotNullable()
            .WithColumn("sales").AsDecimal(15, 2).NotNullable()
            .WithColumn("operating_profit").AsDecimal(15, 2).NotNullable()
            .WithColumn("operating_profit_margin").AsDecimal(5, 2).NotNullable()
            .WithColumn("total_asset_turnover").AsDecimal(5, 2).NotNullable()
            .WithColumn("equity_ratio").AsDecimal(5, 2).NotNullable()
            .WithColumn("calculated_at").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime);

        Execute.Sql(@"
            COMMENT ON TABLE financial_analysis_cache IS '財務分析結果キャッシュ';
            COMMENT ON COLUMN financial_analysis_cache.id IS '主キー';
            COMMENT ON COLUMN financial_analysis_cache.fiscal_year IS '会計年度';
            COMMENT ON COLUMN financial_analysis_cache.sales IS '売上高';
            COMMENT ON COLUMN financial_analysis_cache.operating_profit IS '営業利益';
            COMMENT ON COLUMN financial_analysis_cache.operating_profit_margin IS '営業利益率(%)';
            COMMENT ON COLUMN financial_analysis_cache.total_asset_turnover IS '総資産回転率';
            COMMENT ON COLUMN financial_analysis_cache.equity_ratio IS '自己資本比率(%)';
            COMMENT ON COLUMN financial_analysis_cache.calculated_at IS '計算日時';
        ");
    }

    public override void Down()
    {
        Delete.Table("financial_analysis_cache");
    }
}
