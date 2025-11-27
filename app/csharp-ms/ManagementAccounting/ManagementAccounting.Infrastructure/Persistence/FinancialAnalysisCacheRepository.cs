using Dapper;
using ManagementAccounting.Application.Ports.Out;
using ManagementAccounting.Domain.Entities;
using Microsoft.Extensions.Configuration;
using Npgsql;

namespace ManagementAccounting.Infrastructure.Persistence;

/// <summary>
/// 財務分析キャッシュリポジトリ（Dapper 実装）
/// </summary>
public class FinancialAnalysisCacheRepository : IFinancialAnalysisCacheRepository
{
    private readonly string _connectionString;

    public FinancialAnalysisCacheRepository(IConfiguration configuration)
    {
        _connectionString = configuration.GetConnectionString("ManagementAccounting")!;
    }

    public async Task<FinancialAnalysisCache> SaveAsync(FinancialAnalysisCache cache)
    {
        await using var connection = new NpgsqlConnection(_connectionString);

        // UPSERT: 既存レコードがあれば更新、なければ挿入
        var sql = @"
            INSERT INTO financial_analysis_cache
                (fiscal_year, sales, operating_profit, operating_profit_margin,
                 total_asset_turnover, equity_ratio, calculated_at)
            VALUES
                (@FiscalYear, @Sales, @OperatingProfit, @OperatingProfitMargin,
                 @TotalAssetTurnover, @EquityRatio, @CalculatedAt)
            ON CONFLICT (fiscal_year)
            DO UPDATE SET
                sales = EXCLUDED.sales,
                operating_profit = EXCLUDED.operating_profit,
                operating_profit_margin = EXCLUDED.operating_profit_margin,
                total_asset_turnover = EXCLUDED.total_asset_turnover,
                equity_ratio = EXCLUDED.equity_ratio,
                calculated_at = EXCLUDED.calculated_at
            RETURNING id";

        var id = await connection.ExecuteScalarAsync<int>(sql, cache);
        cache.Id = id;
        return cache;
    }

    public async Task<FinancialAnalysisCache?> FindByFiscalYearAsync(int fiscalYear)
    {
        await using var connection = new NpgsqlConnection(_connectionString);

        var sql = @"
            SELECT id, fiscal_year AS FiscalYear, sales, operating_profit AS OperatingProfit,
                   operating_profit_margin AS OperatingProfitMargin,
                   total_asset_turnover AS TotalAssetTurnover,
                   equity_ratio AS EquityRatio, calculated_at AS CalculatedAt
            FROM financial_analysis_cache
            WHERE fiscal_year = @FiscalYear";

        return await connection.QuerySingleOrDefaultAsync<FinancialAnalysisCache>(
            sql, new { FiscalYear = fiscalYear });
    }

    public async Task<List<FinancialAnalysisCache>> FindAllAsync()
    {
        await using var connection = new NpgsqlConnection(_connectionString);

        var sql = @"
            SELECT id, fiscal_year AS FiscalYear, sales, operating_profit AS OperatingProfit,
                   operating_profit_margin AS OperatingProfitMargin,
                   total_asset_turnover AS TotalAssetTurnover,
                   equity_ratio AS EquityRatio, calculated_at AS CalculatedAt
            FROM financial_analysis_cache
            ORDER BY fiscal_year";

        var results = await connection.QueryAsync<FinancialAnalysisCache>(sql);
        return results.ToList();
    }
}
