module SalesManagement.Infrastructure.Repositories.AlternateProductRepository

open System
open System.Threading.Tasks
open Dapper
open Npgsql
open SalesManagement.Domain.Models

/// <summary>
/// JOIN クエリの結果型
/// </summary>
[<CLIMutable>]
type AlternateProductWithInfo = {
    商品コード: string
    商品名: string
    代替商品コード: string
    代替商品名: string
    優先順位: int
}

/// <summary>
/// 代替商品を登録
/// </summary>
let insertAsync (connectionString: string) (alternate: AlternateProduct) : Task<unit> =
    task {
        let sql = """
            INSERT INTO 代替商品 (
                商品コード, 代替商品コード, 優先順位,
                作成日時, 作成者名, 更新日時, 更新者名
            ) VALUES (
                @ProductCode, @AlternateProductCode, @Priority,
                @CreatedAt, @CreatedBy, @UpdatedAt, @UpdatedBy
            )"""

        use connection = new NpgsqlConnection(connectionString)
        let! _ = connection.ExecuteAsync(sql, alternate)
        return ()
    }

/// <summary>
/// 商品コードで代替商品を検索
/// </summary>
let findByProductCodeAsync (connectionString: string) (productCode: string) : Task<seq<AlternateProduct>> =
    task {
        let sql = """
            SELECT
                商品コード AS ProductCode,
                代替商品コード AS AlternateProductCode,
                優先順位 AS Priority,
                作成日時 AS CreatedAt,
                作成者名 AS CreatedBy,
                更新日時 AS UpdatedAt,
                更新者名 AS UpdatedBy
            FROM 代替商品
            WHERE 商品コード = @ProductCode
            ORDER BY 優先順位"""

        use connection = new NpgsqlConnection(connectionString)
        let! results = connection.QueryAsync<AlternateProduct>(sql, {| ProductCode = productCode |})
        return results :> seq<AlternateProduct>
    }

/// <summary>
/// 代替商品を商品情報と共に取得（JOIN クエリ）
/// </summary>
let findAlternatesWithProductInfoAsync (connectionString: string) (productCode: string) : Task<seq<AlternateProductWithInfo>> =
    task {
        let sql = """
            SELECT
                a.商品コード,
                p1.商品正式名 AS 商品名,
                a.代替商品コード,
                p2.商品正式名 AS 代替商品名,
                a.優先順位
            FROM 代替商品 a
            INNER JOIN 商品マスタ p1 ON a.商品コード = p1.商品コード
            INNER JOIN 商品マスタ p2 ON a.代替商品コード = p2.商品コード
            WHERE a.商品コード = @ProductCode
            ORDER BY a.優先順位"""

        use connection = new NpgsqlConnection(connectionString)
        let! results = connection.QueryAsync<AlternateProductWithInfo>(sql, {| ProductCode = productCode |})
        return results :> seq<AlternateProductWithInfo>
    }
