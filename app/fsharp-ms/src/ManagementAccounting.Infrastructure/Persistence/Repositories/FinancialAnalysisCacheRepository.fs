namespace ManagementAccounting.Infrastructure.Persistence.Repositories

open System
open System.Data
open System.Text.Json
open System.Threading.Tasks
open Npgsql
open Dapper
open ManagementAccounting.Domain.Models
open ManagementAccounting.Application.Ports.Out

/// <summary>
/// DB 行モデル（financial_analysis_cache テーブル）
/// </summary>
[<CLIMutable>]
type FinancialAnalysisCacheRow = {
    id: int
    fiscal_year: int
    data_json: string
    ratios_json: string
    cached_at: DateTime
}

/// <summary>
/// 財務分析キャッシュリポジトリ実装（Dapper）
/// </summary>
type FinancialAnalysisCacheRepository(connectionString: string) =

    let createConnection () : IDbConnection =
        new NpgsqlConnection(connectionString)

    let jsonOptions = JsonSerializerOptions(PropertyNamingPolicy = JsonNamingPolicy.CamelCase)

    let toCache (row: FinancialAnalysisCacheRow) : FinancialAnalysisCache =
        let data = JsonSerializer.Deserialize<FinancialData>(row.data_json, jsonOptions)
        let ratios = JsonSerializer.Deserialize<FinancialRatios>(row.ratios_json, jsonOptions)
        {
            Id = Some row.id
            FiscalYear = row.fiscal_year
            Data = data
            Ratios = ratios
            CachedAt = row.cached_at
        }

    interface IFinancialAnalysisCacheRepository with
        member _.SaveAsync(cache: FinancialAnalysisCache) =
            task {
                use connection = createConnection()
                connection.Open()

                let dataJson = JsonSerializer.Serialize(cache.Data, jsonOptions)
                let ratiosJson = JsonSerializer.Serialize(cache.Ratios, jsonOptions)

                // UPSERT: 既存のキャッシュがあれば更新、なければ挿入
                let sql = """
                    INSERT INTO financial_analysis_cache (fiscal_year, data_json, ratios_json, cached_at)
                    VALUES (@FiscalYear, @DataJson, @RatiosJson, @CachedAt)
                    ON CONFLICT (fiscal_year)
                    DO UPDATE SET
                        data_json = @DataJson,
                        ratios_json = @RatiosJson,
                        cached_at = @CachedAt
                    RETURNING id
                """

                let parameters = {|
                    FiscalYear = cache.FiscalYear
                    DataJson = dataJson
                    RatiosJson = ratiosJson
                    CachedAt = cache.CachedAt
                |}

                let! id = connection.ExecuteScalarAsync<int>(sql, parameters)
                return { cache with Id = Some id }
            }

        member _.GetByFiscalYearAsync(fiscalYear: int) =
            task {
                use connection = createConnection()
                connection.Open()

                let sql = "SELECT * FROM financial_analysis_cache WHERE fiscal_year = @FiscalYear"
                let! results = connection.QueryAsync<FinancialAnalysisCacheRow>(sql, {| FiscalYear = fiscalYear |})

                return
                    results
                    |> Seq.tryHead
                    |> Option.map toCache
            }

        member _.DeleteByFiscalYearAsync(fiscalYear: int) =
            task {
                use connection = createConnection()
                connection.Open()

                let sql = "DELETE FROM financial_analysis_cache WHERE fiscal_year = @FiscalYear"
                let! rowsAffected = connection.ExecuteAsync(sql, {| FiscalYear = fiscalYear |})
                return rowsAffected > 0
            }
